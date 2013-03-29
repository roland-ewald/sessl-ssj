/**
 * Copyright (C) 2013 Roland Ewald
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package sessl.ssj

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

import org.junit.Assert._

/**
 * Tests for {@link SSJOutputAnalysis}.
 *
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class SSJOutputAnalysisTest extends FunSpec {

  describe("SSJ integration") {

    it("supports the least-squares method.") {

      var linearFit: Option[List[Double]] = None
      var quadraticFit: Option[List[Double]] = None

      import sessl._
      import sessl.james._
      import sessl.ssj._

      sessl.execute {
        new Experiment with Observation with SSJOutputAnalysis {
          model = "java://examples.sr.LinearChainSystem"
          stopTime = 1.0
          scan("numOfSpecies" <~ (10, 15))
          observe("x" to "S1", "y" ~ "S5")
          observeAt(range(.0, .1, .9))
          withRunResult {
            result =>
              {
                logger.info("Quadratic least-squares coefficients for trajectory:" + leastSquares(result.trajectory("y"), 2))
                linearFit = Some(leastSquares(result.trajectory("y"), 1))
                quadraticFit = Some(leastSquares(result.trajectory("y"), 2))
              }
          }
        }
      }

      assertTrue("Both fits should be defined.", linearFit.isDefined && quadraticFit.isDefined)
      assertEquals("Two coefficients define f(x) = a*x+b", 2, linearFit.get.size)
      assertEquals("Three coefficients define f(x) = a*x^2+b*x+c", 3, quadraticFit.get.size)
    }
  }
}