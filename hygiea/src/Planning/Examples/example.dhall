-- NOTE: error messages aren't great for type errors
-- TODO: inject NonNeg type as you did with OutputData, then add type
-- annotations. Note the field names do get checked though.
let output = { tags = ["is_funny"], time = [9] }

in output
