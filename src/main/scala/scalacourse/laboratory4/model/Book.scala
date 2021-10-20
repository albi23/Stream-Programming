package scalacourse.laboratory4.model

final class Book(private val title: String) {
  private val hash: Int = Seq(title).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[Book] && this.title == other.asInstanceOf[Book].title

  override def hashCode(): Int = hash

  override def toString: String = f"Book{title=$title}"

}
