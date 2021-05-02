package laboratory4.model

class Book(val title: String) {

  override def equals(other: Any): Boolean =
    other.isInstanceOf[Book] && this.title == other.asInstanceOf[Book].title

  override def hashCode(): Int = {
    Seq(title).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = f"Book{title=$title}"

}
