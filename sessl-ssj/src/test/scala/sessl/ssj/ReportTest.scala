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

/**
 * Tests for {@link SSJOutputAnalysis}.
 *
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class ReportTest extends FunSpec {

  describe("Reporting with SSJ") {

    import sessl._
    import sessl.ssj._

    it("supports scatter plots.") {
      sessl.execute {
        new SSJTestExperiment with Report {
          reportName = "SSJ-Report"
          withRunResult {
            results =>
              {
                reportSection("Run" + results.id) {
                  scatterPlot(results.values("x"), results.values("y"))(title = "The trajectories of x and y!")
                }
              }
          }
        }
      }
    }
  }

}