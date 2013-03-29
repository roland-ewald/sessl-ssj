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
import umontreal.iro.lecuyer.functions.MathFunction

import sessl._
import sessl.james._
import sessl.ssj._

/**
 * Tests for {@link SSJOutputAnalysis}.
 *
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class SSJOutputAnalysisTest extends FunSpec {

  describe("SSJ-based output analysis") {

    def applyFunc(f: MathFunction, times: List[Double]): List[Double] = times.map(f.evaluate(_))

    it("supports the least-squares polynom fitting.") {
      var linearFit: Option[Array[Double]] = None
      var quadraticFit: Option[Array[Double]] = None
      sessl.execute {
        new SSJTestExperiment {
          withRunResult {
            result =>
              {
                val tr = result.trajectory("y")
                linearFit = Some(fitPolynom(result.trajectory("y"), 1).getCoefficients)
                quadraticFit = Some(fitPolynom(result.trajectory("y"), 2).getCoefficients)
                logger.info("Trajectory to be fitted:" + tr)
                logger.info("Quadratic polynom fitted with least-squares evaluated for trajectory:" + applyFunc(fitPolynom(tr, 2), result.times("y")))
              }
          }
        }
      }
      assertTrue("Both fits should be defined.", linearFit.isDefined && quadraticFit.isDefined)
      assertEquals("Two coefficients define f(x) = a*x+b", 2, linearFit.get.size)
      assertEquals("Three coefficients define f(x) = a*x^2+b*x+c", 3, quadraticFit.get.size)
    }

    it("supports fitting B-splines.") {
      var bSpline: Option[MathFunction] = None
      var approxBSpline: Option[MathFunction] = None
      sessl.execute {
        new SSJTestExperiment {
          withRunResult {
            result =>
              {
                val tr = result.trajectory("y")
                bSpline = Some(fitBSpline(tr, 2))
                approxBSpline = Some(fitApproxBSpline(tr, 2, 3))
                logger.info("Trajectory to be fitted:" + tr)
                logger.info("BSpline approximation for this trajectory:" + applyFunc(approxBSpline.get, result.times("y")))
              }
          }
        }
      }
      List(bSpline, approxBSpline).map(x => assertTrue(x.isDefined))
    }

    it("supports fitting cubic splines.") {
      var cubicSpline: Option[MathFunction] = None
      sessl.execute {
        new SSJTestExperiment {
          withRunResult {
            result =>
              {
                val tr = result.trajectory("y")
                cubicSpline = Some(fitCubicSpline(tr, .5))
                logger.info("Trajectory to be fitted:" + tr)
                logger.info("Cubic spline approximation for this trajectory:" + applyFunc(cubicSpline.get, result.times("y")))
              }
          }
        }
      }
      assertTrue(cubicSpline.isDefined)
    }
  }
}

object SSJOutputAnalysisTest {
  val observationRange = range(.0, .1, .9)
}

/** The test experiment to be used to generate some data. */
class SSJTestExperiment extends Experiment with Observation with SSJOutputAnalysis {
  model = "java://examples.sr.LinearChainSystem"
  stopTime = 1.0
  scan("numOfSpecies" <~ (10, 15))
  observe("x" to "S1", "y" ~ "S5")
  observeAt(SSJOutputAnalysisTest.observationRange)
}