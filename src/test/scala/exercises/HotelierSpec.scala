package exercises

import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv

class HotelierSpec(implicit ee: ExecutionEnv) extends Specification {

  def is = "HotelierSpec".title ^
    s2"""
     check HotelierSpec if:
    `Hotelier.generate` should return hotel room status based on events             $e1
    """

  def e1 = {
    Hotelier.generate("LLRL1RL1") must be_===(1010000011)
    Hotelier.generate("L0L0LLRR9") must be_===(1100000010)
    val strings = "LLLLLL" + Array.fill(1000000)("L5R9").mkString("")
    Hotelier.generate(strings) must be_===(1111101000)
  }
}
