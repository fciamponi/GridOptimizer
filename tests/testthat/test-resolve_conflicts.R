test_that("resolve_conflicts resolves adjacency conflicts", {
  box <- matrix(c("cond1_G1_1", "cond1_G1_2", 'cond_G2_1',
                  'cond1_G3_1',"cond1_G2_2", 'cond_G4_1'),
                nrow = 3,
                byrow = TRUE)

  # Initial conflicts
  expect_false(validate_conflicts(box))

  # Resolve conflicts
  resolved_box <- resolve_conflicts(box,max_iterations = 1000)
  expect_true(validate_conflicts(resolved_box))
})
