---
title: some-title
tags: [these, are tags]
---

## Definition

```haskell

BuildDiscontinuation makeAssessmentInterval predicate allowableGap = define
    ()

defDiscontinuation = define
  (\i events ->
    events
      |> filter (getPredicate (containsConcepts ["is_pcsk9"])) -- TODO: are these the only concepts?
  -- combine any concurring intervals
      |> combineIntervals
  -- find gaps between any enrollment intervals (as well as bounds of followup)
      |> gapsWithin (followupIntervalMax i)
  -- get the first gap longer than 30 days (if it exists)
  -- TODO: this logic is NOT correct yet (just copy/pasted from defDisenrollment)
      |> \x ->
           (headMay . filter (\x -> duration x > 30))
             =<< x
  -- Shift endpoints of intervals so that end of follow up is reference point
             |>  fmap (diffFromBegin (followupIntervalMax i))
  -- take the end of this gap as the time of disenrollment
             |>  fmap end
             |>  mkEventTime
  )

```