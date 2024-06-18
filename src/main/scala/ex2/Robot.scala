package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot,var Battery: Int, var Cost: Int ) extends Robot:
  export robot.{position, direction}
  private def CheckBattery(): Boolean =
    if (Cost >= Battery) {
      false
    } else {
      Battery = Battery - Cost;
      true
    }
  def turn(dir: Direction): Unit =
    if (CheckBattery())
      robot.turn(dir)
    else
      println("Not allowed. Battery too low")
  def act(): Unit =
    if (CheckBattery())
      robot.act()
    else
      println("Not allowed. Battery too low")

class RobotCanFail(val robot: Robot,var ProbToFail: Double) extends Robot:
  export robot.{position, direction}
  private def IsFailed(): Boolean =
    Math.random() <= ProbToFail
  def turn(dir: Direction): Unit =
    if (IsFailed())
      println("Failed")
    else
      robot.turn(dir)
  def act(): Unit =
    if (IsFailed())
      println("Failed")
    else
      robot.act()

class RobotRepeated(val robot: Robot,var NumRepeat: Int) extends Robot:
  export robot.{position, direction}
  def turn(dir: Direction): Unit =
    for i <- 0 to NumRepeat yield robot.turn(dir)
  def act(): Unit =
    for i <- 0 to NumRepeat yield robot.act()
  override def toString: String = s"robot at $position facing $direction"


@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

  val BatRobot = RobotWithBattery(SimpleRobot((0,0),Direction.North),10, 3 )
    BatRobot.act()
    BatRobot.turn(robot.direction.turnLeft)
    BatRobot.act()
    BatRobot.act()

  val FailRobot = RobotCanFail(SimpleRobot((0,0),Direction.North),0.5 )
    FailRobot.turn(robot.direction.turnLeft)
    FailRobot.act()
    FailRobot.act()
    FailRobot.act()

  val RepeatRobot = RobotRepeated(SimpleRobot((0,0),Direction.North),1)
    RepeatRobot.turn(robot.direction.turnLeft)
    println(RepeatRobot)
    RepeatRobot.act()
    println(RepeatRobot)
    RepeatRobot.act()
    println(RepeatRobot)
