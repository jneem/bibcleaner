package bibtex

import collection.immutable.HashMap
import Name.apply
import scala.Predef.Map.apply

class PlainBibtexEntry(val entryType: String, key: String, props: Map[String, String])
  extends Map[String, String] with BibtexEntry {
  
  override def get(key: String) = props.get(key)
  override def iterator = props.iterator
  override def -(key: String) = props - key
  override def +[A >: String](kv: (String, A)) = props + kv


}

object PlainBibtexEntry {
  def apply(entryType: String, key: String, props: Iterable[(String, String)]) = {
    // Make the keys all lowercase, since the keys are case insensitive.
    val normalizedProps = props map (x => (x._1.toLowerCase, x._2))
    new PlainBibtexEntry(entryType, key, new HashMap ++ normalizedProps)
  }
}

