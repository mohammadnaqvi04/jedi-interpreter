package context

import expression.*
import value.*

import scala.collection.mutable.ArrayBuffer


object alu:

  def execute(opcode: Identifier, args: List[Value]): Value =
    opcode.name match
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args)
      case "more" => more(args)
      case "equals" => equals(args)
      case "unequals" => unequals(args)
      case "not" => not(args)
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)

      case _ => throw new UndefinedException(opcode)



  private def add(args: List[Value]): Value =
    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: Addable => helper(result + h, unseen.tail)
        case _ => throw TypeException("Inputs to + must be addable")

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match
      case n: Addable => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")

  private def mul(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result * h, unseen.tail)
        case _ => throw TypeException("Inputs to * must be numeric")

    if(args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match
      case n: value.Numeric => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to * must be numeric")

  private def sub(args: List[Value]): Value =
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: Numeric => helper(result - h, unseen.tail)
        case _ => throw TypeException("Inputs to - must be numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match
      case n: Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to - must be numeric")

  private def div(args: List[Value]): Value =
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: Numeric => helper(result / h, unseen.tail)
        case _ => throw TypeException("Inputs to / must be numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match
      case n: Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to / must be numeric")

  private def less(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by <")
    args(0) match
      case x: Ordered[Value] => Boole(x < args(1))
      case _ => throw TypeException("Inputs to < must be orderable")

  private def same(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by ==")
    Boole(args(0) == args(1))

  private def more(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by >")
    args(0) match
      case x: Ordered[Value] => Boole(x > args(1))
      case _ => throw TypeException("Inputs to > must be orderable")

  private def equals(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by ==")
    Boole(args(0) == args(1))

  private def unequals(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by !=")
    Boole(args(0) != args(1))

  private def not(args: List[Value]): Value =
    if (args.size != 1) throw new TypeException("1 input required by !")
    args(0) match
      case b: Boole => !b
      case _ => throw TypeException("Input to ! must be a Boole")

  private def dereference(args: List[Value]): Value =
    if (args.size != 1) throw new TypeException("Dereference takes only one variable")
    if (!args(0).isInstanceOf[Variable]) throw new TypeException("Dereference only works on variables")
    args(0).asInstanceOf[Variable].inf

  private def makeVar(args: List[Value]): Value =
    Variable(args(0))

  private def write(args: List[Value]): Value =
    println(args(0))
    Notification.DONE

  private def prompt(args: List[Value]): Value =
    print("=> ")
    Notification.DONE

  private def read(args: List[Value]): Value =
    val result = io.StdIn.readDouble()
    Inexact(result)

  private def store(args: List[Value]): Value =
    Store(args.to(ArrayBuffer))

  private def put(args: List[Value]) =
    if (args.size != 3 || !args(1).isInstanceOf[Exact] || !args(2).isInstanceOf[Store])
      throw new TypeException("Must be in form: put(v: Value, p: Integer, s: Store)")

    args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Exact].value)
    Notification.DONE

  private def rem(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store])
      throw new TypeException("Must be in form: rem(p: Integer, s: Store)")

    args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer])
    Notification.DONE

  private def get(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Exact] || !args(1).isInstanceOf[Store])
      throw new TypeException("Must be in form: get(p: Integer, s: Store)")

    args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Exact].value)

  private def map(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("Must be in form: map(f: Closure, s: Store)")

    args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])

  private def filter(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("Must be in form: filter(f: Closure, s: Store)")

    args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])

  private def contains(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store])
      throw new TypeException("Must be in form: contains(v: Value, s: Store)")

    args(1).asInstanceOf[Store].contains(args(0))

  private def addLast(args: List[Value]): Value =
    if (args.size != 2 || !args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store])
      throw new TypeException("Must be in form: addLast(v: Value, s: Store)")

    args(1).asInstanceOf[Store].add(args(0))
    Notification.DONE

  private def size(args: List[Value]): Value =
    if (args.size != 1 || !args(0).isInstanceOf[Store])
      throw new TypeException("Must be in form: size(s: Store)")

    val storeSize = args(0).asInstanceOf[Store].size
    Exact(storeSize)


// etc.



