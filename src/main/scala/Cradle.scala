object Cradle {

  val TAB = '\t'
  var Look: Char = _

  def GetChar(): Unit = Look = sys.process.stdin.read().asInstanceOf[Char]
  def Error(s: String): Unit = sys.process.stderr.println(s"Error: $s.")
  def Abort(s: String): Unit = { Error(s); sys.exit(1) }
  def Expected(s: String): Unit = Abort(s"$s Expected")
  def Match(x: Char): Unit = if(Look == x) GetChar else Expected(s"'$x'");
  def IsAlpha(c: Char): Boolean = c.isLetter
  def IsDigit(c: Char): Boolean = c.isDigit
  def IsAddOp(c: Char): Boolean = Array('+','-').contains(c)
  def GetName(): Char = if(!IsAlpha(Look)) {
    Expected("Name")
    'a'
  } else {
    val result = Look.toUpper
    GetChar()
    result
  }

  def GetNum(): Char = if(!IsDigit(Look)) {
    Expected("Name")
    'a'
  } else {
    val result = Look.toUpper
    GetChar()
    result
  }

  def Emit(s: String): Unit = print(s"$TAB $s")
  def EmitLn(s: String): Unit = { Emit(s); println() }
  def Init(): Unit = GetChar()

  def Add(): Unit = { Match('+'); Term(); EmitLn("ADD (SP)+,D0") }
  def Subtract(): Unit = { Match('-'); Term(); EmitLn("SUB (SP)+,D0"); EmitLn("NEG D0") }
  def Multiply(): Unit = { Match('*'); Factor(); EmitLn("MULS (SP)+,D0") }
  def Divide(): Unit = { Match('/'); Factor(); EmitLn("MOVE (SP)+,D1"); EmitLn("DIVS D1,D0") }

  def Factor(): Unit = if(Look == '(') {

    Match('(')
    Expression()
    Match(')')
  } else {
    EmitLn(s"MOVE #${GetNum()},D0")
  }

  def Term(): Unit = {

    Factor()
    while(Array('*','/').contains(Look)) {

      EmitLn("MOVE D0,-(SP)")
      Look match {
        case '*' => Multiply()
        case '/' => Divide()
        case _ => Expected("Mulop")
      }
    }
  }

  def Expression(): Unit = {

    if(IsAddOp(Look)) EmitLn("CLR D0") else Term()
    while(IsAddOp(Look)) {

      EmitLn("MOVE D0,-(SP)")
      Look match {
        case '+' => Add()
        case '-' => Subtract()
        case _ => Expected("Addop")
      }
    }
  }

  def main(args: Array[String]) {

    Init()
    Expression()
  }
}
