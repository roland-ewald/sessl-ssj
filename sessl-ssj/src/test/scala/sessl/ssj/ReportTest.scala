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

import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import java.io.File

/**
 * Tests for {@link SSJOutputAnalysis}.
 *
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class ReportTest extends FunSpec {

  val testReportName = "SSJ-Report"

  val plotNames = Array("Scatterplot", "Histogram", "Lineplot")

  def fileFor(n: String) = new File("./" + testReportName + "-" + n + ".tex")

  describe("Reporting with SSJ") {

    import sessl._
    import sessl.ssj._

    it("supports different plots.") {

      plotNames.foreach(fileFor(_).delete)

      sessl.execute {
        new SSJTestExperiment with Report {
          reportName = testReportName
          withRunResult {
            results =>
              {
                reportSection(plotNames(0)) {
                  scatterPlot(results.values("x"), results.values("y"))(title = "A scatterplot")
                }
                reportSection(plotNames(1)) {
                  histogram(results.values("x"))(title = "A histogram", xLabel = "the x label", yLabel = "y-label")
                }
                reportSection(plotNames(2)) {
                  linePlot(results ~ ("x"), results ~ ("y"))(title = "A lineplot for x and y trajectories.")
                }
              }
          }
        }
      }

      plotNames.foreach(n => {
        assertTrue(fileFor(n).exists)
      })

    }
  }

}