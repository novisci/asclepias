# Changelog for edm

## 0.23.2

* Adds `previewBenefit`, `previewBenefitE`, `previewExchange`, and `previewExchangeE` for accessing the `benefit` or `exchange` field from a `Plan` within the `Enrollment` or `Eligibility` `Domain`s. Functions ending in `E` take an `Event` as input.
* Adds the `Provider` fact to align with EDM v1.2. Adds corresponding field to the facts of the `Diagnosis`, `Medication`, and `Procedure` domains.
* Adds `previewCode` and `previewCodeE` for accessing the `Text` part of a `Code` (i.e. the code without the codebook).

## 0.23.1

* Makes the `Enrollment` domain's plan fact optional, in accordance with EDM v1.2.
* Adds `previewDaysSupply` function for accessing (maybe) from a medication domain.

## 0.23.0

* Makes fundamental changes to intervals are marshaled into Haskell from event data JSON. The short version is that a moment is added to every `end`. A reason for this change is that the event data `time` object does not necessary represent a proper interval. Consider, for example, the following time objects:

```json
{"time":{"begin":"2015-01-01","end":"2015-01-01"}}
{"time":{"begin":"2015-01-01","end":"2015-01-02"}}
```

In speaking with our scientists, they would consider the first case to be 1 day and the second to be 2 days. If the begin and end we directly parsed into an interval, however, the first case should fail and the second would be an interval of 1 day. A solution is simply to add a moment to the ends, which is what I did. In doing so, I created a new type `EDMInterval a` that simply wraps an `Interval a`. And in the `FromJSON (EDMInterval a)` instance, I do the adding of the moment. The `FromJSON (Interval a)` instance is removed altogether, thus allowing developers who want to marshal a different JSON data structure that represents valid intervals to do so.

The behavior when the `end` is missing is unchanged: an interval of a moment's duration is created from the `begin`. For example, `decode`ing

```json
{"time":{"begin":"2015-01-01","end":null}}
```

yields `Just (EDMInterval {getEDMInterval = (2015-01-01, 2015-01-02)})`. Also, if after adding a moment to `end`, `end <= begin` results in a parse error.

* Updates `interval-algebra` dependency to version 1.1.0.

## 0.22.0

* Adds much of the remaining `event-data-model` Domains and Facts to align with v1.1 of the EDM. *Caveat emptor*: Do not assume that all domains are implemented, nor that those are implemented are implemented faithfully to the EDM schema. Check that the domain you seek is in `EventData.Context.Domain` module and that it has been added to the `FromJSON Domain` instance in the `EventData.Aeson` module.
* Adds field (`table`, `file`, etc) to `Source` type and adds corresponding `FromJSON` instance.

## 0.21.0

* Updates `interval-algebra` dependency to `1.0.0`.
* Replaced `lens` dependency with `generic-lens` and `microlens`. Now, any type for which `Generic` is derived lens and prisms come along for free! Updates the `EventDataModel.Accessors` module accordingly.
* Become a separate package from `hasklepias`.
