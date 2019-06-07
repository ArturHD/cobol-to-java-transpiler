package de.netherspace.apps.actojat

import org.hamcrest.MatcherAssert.assertThat
import org.junit.Test
import org.hamcrest.Matchers.`is` as Is

class AppTest {

    @Test
    fun trivialTest() {
        val b = true
        assertThat(b, Is(true))
    }

    // TODO: test the CLI!

}
