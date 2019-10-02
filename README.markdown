# Vlad

Citrine Informatics Take-Home Challenge

The friendly decimal representation in the example response is implemented,
but of course it breaks down when the `multiplication_factor` gets very large
or small. There is a commented-out line in `convertUnits` that can be used to
instead return a `multiplication_factor` in scientific notation that actually
has 14 significant figures.

`scalafmt` is used throughout except for the case "table" in `UnitConverter.scala`, which is preserved for readability.

Errors are passed through as the `unit_name` with a `multiplication_factor` of `0.0`.