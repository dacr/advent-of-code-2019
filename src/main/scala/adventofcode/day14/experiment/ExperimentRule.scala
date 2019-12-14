package adventofcode.day14.experiment

import org.kie.api.KieServices
import org.optaplanner.core.api.domain.entity.PlanningEntity
import org.optaplanner.core.api.domain.solution.PlanningSolution
import org.optaplanner.core.api.solver.{Solver, SolverFactory}

@PlanningSolution
class ExperimentSolution {

}

@PlanningEntity
class ExperimentRule {
}

object ExperimentRule {

  def main(args: Array[String]): Unit = {
    val solverConfigResource = "adventofcode/day14/experiment/experimentSolverConfig.xml"
    val solverFactory:SolverFactory[ExperimentSolution] = SolverFactory.createFromXmlResource(solverConfigResource, getClass().getClassLoader())
    val solver:Solver[ExperimentSolution] = solverFactory.buildSolver()
    //val bestSolution:ExperimentSolution = solver.solve(problem)
  }
}
