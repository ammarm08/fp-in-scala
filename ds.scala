object DS {
  def tail[A](as: List[A]) = as match {
    case Nil => Nil
    case h :: t => t
  }
}