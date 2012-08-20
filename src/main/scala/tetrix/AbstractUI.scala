package tetrix

class AbstractUI {
  private[this] var lastKey = ""

  def left() {
    lastKey = "left"
  }

  def right() {
    lastKey = "right"
  }

  def up() {
    lastKey = "up"
  }

  def down() {
    lastKey = "down"
  }

  def space() {
    lastKey = "space"
  }

  def last: String = lastKey
}
