package algorithm

import org.scalatest.FunSuite

case class HashTable[KEY, VALUE: Manifest]() {
  private val values = Array.fill[Option[VALUE]](100)(None)

  // direct-address table?

  def apply(key: KEY): VALUE = {
    val hash0 = hash(key)
    println("hash", hash0)
    values(hash0).get
  }

  def update(key: KEY, value: VALUE) {
    val hash0 = hash(key)
    println("hash", hash0)
    values(hash0) = Some(value)
  }

  def delete(key: KEY) {
    val hash0 = hash(key)
    values(hash0) = None
  }

  private def hash(key: KEY) = key.hashCode() % values.size
}

class HashTableTest extends FunSuite {

  // ハッシュする
  // hashCode は n になります
  case class Key(override val hashCode: Int) {
    // override def hashCode(): Int = n

    override def equals(obj: Any): Boolean =
      obj match {
        case o: Key => this eq o
        case _ => false
      }
  }

  test("insert/search ハッシュがかぶらない場合") {
    val map = HashTable[Key, Int]()
    val key1 = Key(hashCode = 1)
    val key2 = Key(hashCode = 2)
    val key3 = Key(hashCode = 3)

    map(key1) = 10
    map(key2) = 20
    assert(map(key1) === 10, "入れたものは入ってます")
    assert(map(key2) == 20, "入れたものは入ってます")

    // "入れてないものは入ってないです"
    intercept[Exception] {
      map(key3)
    }
  }

  test("insert/search ハッシュがかぶる場合") {
    val map = HashTable[Key, Int]()
    val key1 = Key(hashCode = 1)
    val key2 = Key(hashCode = 1)

    map(key1) = 10
    map(key2) = 20
    assert(map(key1) === 10, "入れたものは入ってます")
    assert(map(key2) == 20, "入れたものは入ってます")
  }

  test("delete") {
    val map = HashTable[Key, Int]()
    val key1 = Key(hashCode = 1)
    val key2 = Key(hashCode = 2)

    map(key1) = 10
    map(key2) = 20
    map.delete(key1)
    assert(map(key2) === 20, "入れたものは入ってます")

    // 削除したものは入っていないです
    intercept[Exception] {
      map(key1)
    }
  }
}
