package de.netherspace.apps.actojat.intermediaterepresentation.java;

public class IrFactory {

  /**
   * Creates a new IR Program object.
   *
   * @return the newly created Program
   */
  public static Program createProgram() {
    return new Program();
  }

  /**
   * Creates a new IR Assignment object.
   *
   * @return the newly created Assignment
   */
  public static Assignment createAssignment() {
    final LeftHandSide lhs = new LeftHandSide();
    final Assignment assignment = new Assignment(lhs);
    return assignment;
  }

}
