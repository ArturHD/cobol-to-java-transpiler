package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Import extends JavaLanguageConstruct {

  private String name;


  /**
   * Default constructor.
   */
  public Import() {
    super();
  }


  /**
   * Auxiliary constructor.
   *
   * @param name the import's name
   */
  public Import(String name) {
    this.name = name;
  }

}
