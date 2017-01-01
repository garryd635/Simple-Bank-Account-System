/**Simple Bank
*Garry Dominique
*January 1, 2017
*/
import scala.collection.mutable.ListBuffer
import scala.io.Source
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import java.io._

/**Contains the functions for running and performing the simple bank tasks.*/
object bank{
	
	/**Capture the transaction amount from user input and checks to see if the input is valid before
	* sending the data to readFile().
	*
	* @params pCmd The type of transaction the user is making (deposit/withdrawal)
	*	
	*/
	def transactions(pCmd: String) {
		val cmd = pCmd //Sets transaction type
		var amt = scala.io.StdIn.readLine("Enter the amount you wish to " + cmd + ".") //Asks user for the transaction amount.
		var amtLst = amt.toString().toList //Spilts the amount into a list of chars.
		//Inital booleans for validation checking
		var proper = false
		var lessDeci = false
		var decimalExist = false
		//Keep track of index while going through for-loop.
		var index = 0


		/** Validation: Input format
				*  
				*  Check to see if the amount is a decimal that goes beyond the hundreadth place.
				*  The amount is a proper decimal if the decimal point exists, the thousandth place does not exist, 
				*  and the tenths and hundredths place has a value.
				*/
		for( x <- amtLst){		
			if (x == '.'){	
				decimalExist = true
				/** Check to see if thousandth place exist.  If it does not, then the 
				*		IndexOutOfBoundsException should be caught, thus making the amount a proper xx.xx decimal.
				*/	
				try{
					var test1 = amtLst(index + 3)
				}
				catch{
					case e: IndexOutOfBoundsException => proper = true
				}

				/**Check to make sure the tenths and hundredth place has a value.  The value does not exist if 
				*  the IndexOutOfBoundsException is false.
				*/
				try{
					var test2 = amtLst(index + 1)
					var test3 = amtLst(index + 2)
				}
				catch{
					case e: IndexOutOfBoundsException => lessDeci = true
				}

				/**Prompts the user that the amount is not in the proper format and restarts the 
				*  transactions() method if proper remains false (there's a value in the thousandths place), or lessDeci
				*  becomes true (There is no value in the tenths or hundredth place) as the amount 
				*	 is not valid.
				*/
				if (proper == false || lessDeci == true){
					println("Please enter the amount with two decimal places")
					transactions(cmd)
				}
			}
			index = index + 1 //Increments index
		}
		
		/**Validation: Check if amount is numerical
		*		Check to see if the decimal is a double. If the amount is not a double, call the handleNumFormat 
		*   method.  Otherwise, continue.
		*/
		try{
			amt.toDouble
		}
		catch{
			case e: NumberFormatException => handleNumFormat(cmd)
		}

		/**Prompts user that the input is not in the proper format and starts the transactions() method 
		*  if the decimalExist remains false.
		*/
		if(decimalExist == false){
			println ("Please enter the amount in decimal xx.xx form.")
			transactions(cmd)
		}
		//Passes the amount and transaction type to readFile as the input is valid.
		readFile(amt, cmd)		
	}//transactions()


	/** Informs user that the amount from the transaction method is not
	*  numerical.  It also restarts the transaction method.*/
	def handleNumFormat(cmd: String){
		println("Please enter the amount in a numerical format.")
		transactions(cmd)
	}//handleNumFormat()


	/**Reads the log.html file and updates the file if the transaction
	*  type is either deposit or withdrawal.
	*
	*  @param amt transaction amount
	*  @param cmd type of transaction
	*/
	def readFile(amt: String, cmd: String){
		// store the html lines from log.html into the string builder
		val buf =new StringBuilder
		val filename = "log.html"

		for (line <- Source.fromFile(filename).getLines()){
			buf ++= line
		}
		
		//Parse the html using Jsoup and select the transactions table.
		val html = buf.toString()
		val doc = Jsoup.parse(html)//stores html document
		val txt = doc.body().text()//stores the text from the html
		val ele = doc.select("#transactions tbody")//select transaction table

		//If transaction type is deposit, append a new row to the table
		if (cmd == "deposit"){
			ele.append("<tr><td>" + amt.toString() + "</td></tr>")
		}
		//If the transaction type is withdrawal, append the new row and make the 
		//value negative
		if (cmd == "withdrawal"){
			ele.append("<tr><td>-" + amt.toString() + "</td></tr>")
		}
		
		//Update html.log if the transaction type is deposit/withdrawal
		if (cmd == "deposit" || cmd == "withdrawal"){
			val file = new File(filename)
			val bw = new BufferedWriter(new FileWriter(file))
			bw.write(doc.toString())
			bw.close()
			println("Transaction Successfully saved to log.html")
		}

		//Print out the total of the balance if transaction type is balance
		if (cmd == "balance"){
			val splitText = txt.split(" ")//split the txt string
			//Initial balance and index count
			var balance = 0.00
			var index = 0
			//Add the amount to balance 
			for ( word <- splitText){
				if (word != "Amount"){
					balance = balance + word.toDouble
				}
			}

			//Makes sure balance is rounded at two decimal places
			balance = ((balance * 100).round / 100.toDouble)
			//Prints the current balance
			println ("The current balance is: $" + balance.toString())
		}
		//Go back to the main menu
		mainMenu()
	}//readFile()

	/**Prompts the user to enter a command of a deposit, withdrawal, balance, or exiting the application.
	*  The function will then start the appropriate function (transactions() for deposit/withdrawal, readFile() for
	*	 balance, and nothing for exiting the system.
	*
	 */
	def mainMenu(){
		//Prompt the user to input a command
		var command = scala.io.StdIn.readLine("Please enter a command (Deposit, Withdrawal, Balance, Exit): ")
		//Lowercase the user's input to allow case-insenstivity
		var newCommand = command.toLowerCase()
		//Starts the appropriate method if command is either deposit, withdrawal, balance, or exit, 
		if (newCommand == "deposit" || newCommand == "withdrawal" || newCommand == "balance" || newCommand == "exit"){
			if(newCommand == "deposit" || newCommand == "withdrawal"){
				transactions(newCommand)
			}
			if(newCommand == "balance"){
				readFile("0",newCommand)//Ammount does not matter as the method will only calculate a balance.
			}
			if(newCommand == "exit"){
				
			}
		}
		//Informs the user and restarts the main menu if the user's input is not correct.
		else{
			println("Input Error: Please enter the word: deposit, withdrawal, balance, or exit")
			mainMenu()
		}
	}//mainMenu

	/**Runs the main menu at the application's startup*/
	def main(args: Array[String]) {
		mainMenu()
	}
}