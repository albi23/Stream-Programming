package laboratory6

import java.io.Serializable

object HashMethod extends Enumeration {

  abstract class HashMethodImpl extends Val {
    def getHashFunction: HashFunction
  }

  final val RNG = new HashMethodImpl() {
    override def getHashFunction: HashFunction = HashProvider.hashRNG
  }

  final val SHA256 = new HashMethodImpl() {
    override def getHashFunction: HashFunction = (bytes, m, k) =>
      HashProvider.hashCrypt(bytes, m, k, "SHA-256")
  }

  final val SHA384 = new HashMethodImpl() {
    override def getHashFunction: HashFunction = (bytes, m, k) =>
      HashProvider.hashCrypt(bytes, m, k, "SHA-384")
  }

  final val SHA512 = new HashMethodImpl() {

    override def getHashFunction: HashFunction = (bytes, m, k) =>
      HashProvider.hashCrypt(bytes, m, k, "SHA-512")
  }


  trait HashFunction extends Serializable {
    def hash(bytes: Array[Byte], size: Int, hashes: Int): Array[Int]
  }
}
