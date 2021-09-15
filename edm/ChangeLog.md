# Changelog for hasklepias

## 0.21.0

* Replaced `lens` dependency with `generic-lens` and `microlens`. Now, any type for which `Generic` is derived lens and prisms come along for free! Updates the `EventDataModel.Accessors` module accordingly.
* Become a separate package from `hasklepias`.
