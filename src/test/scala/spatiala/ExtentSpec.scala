package spatiala

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ExtentSpec extends FunSpec
                    with ShouldMatchers {
  describe("areaIncreaseToFit") {
    it("should compute correct for contained extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( 2.5, -75.0, 7.5,-25.0)

      e1.areaIncreaseToFit(e2) should be (0.0)
    }

    it("should compute correct for west-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0, 0.0)
      val e2 = Extent( -5.0, -75.0, 7.5,-25.0)

      val increase = 5.0 * 100.0
      e1.areaIncreaseToFit(e2) should be (increase)
    }

    it("should compute correct for east-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( 0.0, -75.0, 15.0,-25.0)

      val increase = 5.0 * 100.0
      e1.areaIncreaseToFit(e2) should be (increase)
    }

    it("should compute correct for north-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( 0.0, -75.0, 10.0,15.0)

      val increase = 15.0 * 10.0
      e1.areaIncreaseToFit(e2) should be (increase)
    }

    it("should compute correct for south-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( 0.0, -115.0, 10.0,-25.0)

      val increase = 15.0 * 10.0
      e1.areaIncreaseToFit(e2) should be (increase)
    }

    it("should compute correct for northwest-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0, 0.0)
      val e2 = Extent( -5.0, -115.0, 7.5,-25.0)

      val increase = (100.0*5) + (15*(10+5))
      e1.areaIncreaseToFit(e2) should be (increase)
    }

    it("should compute correct for northwest-and-southwest-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( 0.0, -115.0, 15.0,17.0)

      val increase = 5*(100+15+17) + (15*10)+(17*10)
      e1.areaIncreaseToFit(e2) should be (increase)
    }

    it("should compute correct for greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( -5.0, -115.0, 17.0,17.0)

      val increase =(100+15+17)*(5+7) + (10*15+10*17)
      e1.areaIncreaseToFit(e2) should be (increase)
    }
  }

  describe("deadSpaceWith") {
    it("should compute correct for contained extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( 2.5, -75.0, 7.5,-25.0)

      e1.deadSpaceWith(e2) should be (0.0)
    }

    it("should compute correct for west-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0, 0.0)
      val e2 = Extent( -5.0, -75.0, 7.5,-25.0)

      val deadSpace = 5.0 * (100 - 50)
      e1.deadSpaceWith(e2) should be (deadSpace)
    }

    it("should compute correct for disjoint extends") {
      val e1 = Extent( 0.0,-100.0,10.0, 0.0)
      val e2 = Extent( 15.0, 75.0, 25.0,25.0)

      val deadSpace = 15.0 * 25.0 + 100.0 * 10.0 + 50.0 * 10.0
      e1.deadSpaceWith(e2) should be (deadSpace)
    }

    it("should compute correct for northwest-and-southwest-greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( 0.0, -115.0, 15.0,17.0)

      val increase = 5*(100+15+17) + (15*10)+(17*10)
      e1.areaIncreaseToFit(e2) should be (increase)
    }

    it("should compute correct for greater extent") {
      val e1 = Extent( 0.0,-100.0,10.0,  0.0)
      val e2 = Extent( -5.0, -115.0, 17.0,17.0)

      e1.deadSpaceWith(e2) should be (0.0)
    }
  }
}
