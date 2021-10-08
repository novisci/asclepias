# Changelog for edm

## 0.23.0

* Updates `interval-algebra` dependency to version 1.1.0.

## 0.22.0

* Adds much of the remaining `event-data-model` Domains and Facts to align with v1.1 of the EDM. *Caveat emptor*: Do not assume that all domains are implemented, nor that those are implemented are implemented faithfully to the EDM schema. Check that the domain you seek is in `EventData.Context.Domain` module and that it has been added to the `FromJSON Domain` instance in the `EventData.Aeson` module.
* Adds field (`table`, `file`, etc) to `Source` type and adds corresponding `FromJSON` instance.

## 0.21.0

* Updates `interval-algebra` dependency to `1.0.0`.
* Replaced `lens` dependency with `generic-lens` and `microlens`. Now, any type for which `Generic` is derived lens and prisms come along for free! Updates the `EventDataModel.Accessors` module accordingly.
* Become a separate package from `hasklepias`.
