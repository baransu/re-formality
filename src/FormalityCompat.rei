[@deprecated "Use Formality instead."];

module Dom = Formality__PublicHelpers.Dom;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module Make = FormalityCompat__Form.Make;

type ok = Formality__Validation.Result.ok = | Valid | NoValue;

type result('message) = Belt.Result.t(ok, 'message);

type status('message) =
  Formality__Validation.Sync.status('message) =
    | Pristine
    | Dirty(
        Formality__Validation.Result.result('message),
        Formality__Validation.Visibility.t,
      );

type validate('state, 'message) =
  'state => Formality__Validation.Result.result('message);

type validator('field, 'state, 'message) =
  Formality__Validation.Sync.validator('field, 'state, 'message) = {
    field: 'field,
    strategy: Formality__Strategy.t,
    dependents: option(list('field)),
    validate: validate('state, 'message),
  };

module Async: {
  module Make = FormalityCompat__FormAsyncOnChange.Make;
  module MakeOnBlur = FormalityCompat__FormAsyncOnBlur.Make;

  let debounceInterval: int;

  type status('message) =
    Formality__Validation.Async.status('message) =
      | Pristine
      | Dirty(
          Formality__Validation.Result.result('message),
          Formality__Validation.Visibility.t,
        )
      | Validating;

  type validate('state, 'message) =
    'state => Js.Promise.t(Formality__Validation.Result.result('message));

  type equalityChecker('state) = ('state, 'state) => bool;

  type validator('field, 'state, 'message) =
    Formality__Validation.Async.validator('field, 'state, 'message) = {
      field: 'field,
      strategy: Formality__Strategy.t,
      dependents: option(list('field)),
      validate: Formality__Validation.Sync.validate('state, 'message),
      validateAsync:
        option((validate('state, 'message), equalityChecker('state))),
    };
};
