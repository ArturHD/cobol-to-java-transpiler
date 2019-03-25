package de.netherspace.apps.actojat;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.Test;

public class AppTest {

  @Test
  public void trivialTest() {
    final boolean b = true;
    assertThat(b, is(true));
  }

  //TODO: test the CLI !
}
