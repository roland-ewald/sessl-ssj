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

import sessl.AbstractExperiment
import sessl.ExperimentConfiguration
import sessl.Trajectory
import umontreal.iro.lecuyer.functions.MathFunction

/**
 * Provides support for output analysis with SSJ.
 *
 * See SSJ manual for documentation: http://www.iro.umontreal.ca/~simardr/ssj/doc/pdf/guidefunctionfit.pdf
 *
 * @author Roland Ewald
 */
trait SSJOutputAnalysis extends ExperimentConfiguration {
  this: AbstractExperiment =>

  def fit[M <: MathFunction](data: Trajectory, af: ApproximationForm[M]): M = SSJOutputAnalysis.fit(data, af)

}

object SSJOutputAnalysis {

  /** Fits a function to the given data. */
  def fit[M <: MathFunction](data: Trajectory, af: ApproximationForm[M]): M = af.fitToData(data)

  /** Fits a function to the given data, evaluates it for the given trajectories' time points. */
  def fitAndEval[M <: MathFunction](data: Trajectory, af: ApproximationForm[M]): Trajectory = {
    val fitted = fit(data, af)
    val times = data.map(_._1)
    times.map(t => (t, fitted.evaluate(t)))
  }

}