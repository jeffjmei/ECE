Generate a simulation + histogram callout for the ECE notes following the project conventions.

The user will describe what they want to simulate. Use $ARGUMENTS as the description.

Produce a green collapsible callout (`{.callout-tip collapse="true"}`) with:

1. A title of the form `## Simulation: Verifying <quantity>` (or "Does <estimator> Recover the Truth?" if checking estimator bias)

2. A short prose paragraph (2–3 sentences) describing: the scenario parameters (n, L, err_type, any non-default sigma), what quantity is being simulated, and what the reader should look for (sample mean near red line).

3. A single `{r}` chunk with `#| echo: false`, `#| cache: true`, `#| message: false` that:
   - Sets up `params` via `scenario(...)` 
   - Extracts the true value for the red line from `params`
   - Runs the simulation using `map_dbl(1:1000, ~ { ... generate_data(params) ... })`
   - Uses `|>` not `%>%`

4. A second `{r}` chunk with `#| echo: false` that calls:
   ```r
   make_sim_hist(sim, true, expression(<appropriate label>))
   ```

Do not include `library()` calls or `devtools::load_all()` — those are handled by the chapter setup chunk.

Ask the user for any missing details (scenario parameters, quantity being estimated, true value expression) before generating if $ARGUMENTS is too vague.
