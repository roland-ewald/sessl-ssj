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

import sessl._
import sessl.AbstractExperiment
import sessl.ExperimentConfiguration
import sessl.util.ScalaToJava._
import umontreal.iro.lecuyer.functionfit.LeastSquares

/**
 * Provides support for output analysis with SSJ.
 *
 * @author Roland Ewald
 */
trait SSJOutputAnalysis extends ExperimentConfiguration {
  this: AbstractExperiment =>

  def leastSquares(data: Trajectory, degree: Int): List[Double] = {
    require(data.head._2.isInstanceOf[Number], "Trajectory values need to be real-valued.")
    val timeArray = data.map(_._1).toArray
    val dataArray = data.map(_._2.asInstanceOf[Number].doubleValue).toArray
    LeastSquares.getCoefficients(timeArray, dataArray, degree).toList
  }

}