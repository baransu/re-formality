module Validation = Formality__Validation;
module Strategy = Formality__Strategy;
module FormStatus = Formality__FormStatus;

module type Form = {
  type field;
  type state;
  type message;
  type submissionError;
  let validators: list(Validation.validator(field, state, message));
};

module Make = (Form: Form) => {
  module FieldId =
    Id.MakeComparable({
      type t = Form.field;
      let cmp = Pervasives.compare;
    });

  type state = {
    input: Form.state,
    status: FormStatus.t(Form.submissionError),
    fields:
      Map.t(Form.field, Validation.status(Form.message), FieldId.identity),
    validators:
      ref(
        Map.t(
          Form.field,
          Validation.validator(Form.field, Form.state, Form.message),
          FieldId.identity,
        ),
      ),
    submittedOnce: bool,
  };

  type action =
    | Change(Form.field, Form.state)
    | Blur(Form.field)
    | Submit
    | SetSubmittedStatus(option(Form.state))
    | SetSubmissionFailedStatus(Form.submissionError)
    | MapSubmissionError(Form.submissionError => Form.submissionError)
    | DismissSubmissionError
    | DismissSubmissionResult
    | Reset;

  type interface = {
    state: Form.state,
    status: FormStatus.t(Form.submissionError),
    result: Form.field => option(Validation.Result.result(Form.message)),
    dirty: unit => bool,
    valid: unit => bool,
    submitting: bool,
    change: (Form.field, Form.state) => unit,
    blur: Form.field => unit,
    submit: unit => unit,
    mapSubmissionError: (Form.submissionError => Form.submissionError) => unit,
    dismissSubmissionError: unit => unit,
    dismissSubmissionResult: unit => unit,
    reset: unit => unit,
  };

  let getInitialState = input => {
    input,
    status: FormStatus.Editing,
    fields:
      Form.validators->List.reduce(
        Map.make(~id=(module FieldId)), (fields, validator) =>
        fields->Map.set(validator.field, Validation.Pristine)
      ),
    validators:
      ref(
        Form.validators->List.reduce(
          Map.make(~id=(module FieldId)), (fields, validator) =>
          fields->Map.set(validator.field, validator)
        ),
      ),
    submittedOnce: false,
  };

  let component = ReasonReact.reducerComponent("Formality.Form");
  let make =
      (
        ~initialState: Form.state,
        ~onSubmit:
           (
             Form.state,
             Validation.submissionCallbacks(Form.state, Form.submissionError)
           ) =>
           unit,
        children,
      ) => {
    ...component,
    initialState: () => initialState->getInitialState,
    reducer: (action, state) =>
      switch (action) {
      | Change(field, input) =>
        let validator = (state.validators^)->Map.get(field);
        switch (validator) {
        | None =>
          ReasonReact.Update({
            ...state,
            input,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })
        | Some(validator) =>
          let status = state.fields->Map.get(field);
          let result = input->(validator.validate);
          let fields =
            switch (validator.dependents) {
            | None => state.fields
            | Some(dependents) =>
              dependents->List.reduce(
                state.fields,
                (fields, field) => {
                  let status = fields->Map.get(field);
                  switch (status) {
                  | None
                  | Some(Pristine)
                  | Some(Dirty(_, Hidden)) => fields
                  | Some(Dirty(_, Shown)) =>
                    let validator = (state.validators^)->Map.getExn(field);
                    fields->Map.set(
                      field,
                      Dirty(input->(validator.validate), Shown),
                    );
                  };
                },
              )
            };
          switch (validator.strategy, status, state.submittedOnce) {
          | (_, Some(Dirty(_, Shown)), _)
          | (_, _, true)
          | (OnFirstChange, _, false) =>
            ReasonReact.Update({
              ...state,
              input,
              fields: fields->Map.set(field, Dirty(result, Shown)),
            })
          | (OnFirstSuccess | OnFirstSuccessOrFirstBlur, _, false) =>
            ReasonReact.Update({
              ...state,
              input,
              fields:
                switch (result) {
                | Ok(Valid | NoValue) =>
                  fields->Map.set(field, Dirty(result, Shown))
                | Error(_) => fields->Map.set(field, Dirty(result, Hidden))
                },
            })
          | (OnFirstBlur | OnSubmit, _, false) =>
            ReasonReact.Update({
              ...state,
              input,
              fields: fields->Map.set(field, Dirty(result, Hidden)),
            })
          };
        };

      | Blur(field) =>
        let status = state.fields->Map.get(field);
        let validator = (state.validators^)->Map.get(field);
        switch (status, validator) {
        | (Some(Dirty(_, Shown)), Some(_) | None)
        | (Some(Dirty(_, Hidden)), None) => ReasonReact.NoUpdate
        | (Some(Pristine) | None, None) =>
          ReasonReact.Update({
            ...state,
            fields: state.fields->Map.set(field, Dirty(Ok(Valid), Hidden)),
          })
        | (Some(Pristine) | None, Some(validator)) =>
          let result = state.input->(validator.validate);
          switch (validator.strategy) {
          | OnFirstChange
          | OnFirstSuccess
          | OnSubmit =>
            ReasonReact.Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Hidden)),
            })
          | OnFirstBlur
          | OnFirstSuccessOrFirstBlur =>
            ReasonReact.Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Shown)),
            })
          };
        | (Some(Dirty(_, Hidden)), Some(validator)) =>
          let result = state.input->(validator.validate);
          switch (validator.strategy) {
          | OnFirstChange
          | OnFirstSuccess
          | OnSubmit =>
            ReasonReact.Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Hidden)),
            })
          | OnFirstBlur
          | OnFirstSuccessOrFirstBlur =>
            ReasonReact.Update({
              ...state,
              fields: state.fields->Map.set(field, Dirty(result, Shown)),
            })
          };
        };

      | Submit =>
        switch (state.status) {
        | Submitting(_) => ReasonReact.NoUpdate
        | Editing
        | Submitted
        | SubmissionFailed(_) =>
          let (valid, fields) =
            (state.validators^)
            ->Map.reduce(
                (true, state.fields),
                ((valid, fields), field, validator) => {
                  let result = state.input->(validator.validate);
                  let fields = fields->Map.set(field, Dirty(result, Shown));
                  switch (valid, result) {
                  | (false, _)
                  | (true, Error(_)) => (false, fields)
                  | (true, Ok(Valid | NoValue)) => (true, fields)
                  };
                },
              );
          if (valid) {
            ReasonReact.UpdateWithSideEffects(
              {
                ...state,
                fields,
                status:
                  FormStatus.Submitting(
                    switch (state.status) {
                    | SubmissionFailed(error) => Some(error)
                    | Editing
                    | Submitted
                    | Submitting(_) => None
                    },
                  ),
                submittedOnce: true,
              },
              ({state, send}) =>
                state.input
                ->onSubmit({
                    notifyOnSuccess: state => SetSubmittedStatus(state)->send,
                    notifyOnFailure: error =>
                      SetSubmissionFailedStatus(error)->send,
                    reset: () => Reset->send,
                    dismissSubmissionResult: () =>
                      DismissSubmissionResult->send,
                  }),
            );
          } else {
            ReasonReact.Update({
              ...state,
              fields,
              status: FormStatus.Editing,
              submittedOnce: true,
            });
          };
        }

      | SetSubmittedStatus(data) =>
        switch (data) {
        | Some(data) =>
          ReasonReact.Update({
            ...state,
            input: data,
            status: FormStatus.Submitted,
            fields: state.fields->Map.map(_ => Validation.Pristine),
          })
        | None =>
          ReasonReact.Update({
            ...state,
            status: FormStatus.Submitted,
            fields: state.fields->Map.map(_ => Validation.Pristine),
          })
        }

      | SetSubmissionFailedStatus(error) =>
        ReasonReact.Update({
          ...state,
          status: FormStatus.SubmissionFailed(error),
        })

      | MapSubmissionError(map) =>
        switch (state.status) {
        | Submitting(Some(error)) =>
          ReasonReact.Update({
            ...state,
            status: Submitting(Some(error->map)),
          })
        | SubmissionFailed(error) =>
          ReasonReact.Update({
            ...state,
            status: SubmissionFailed(error->map),
          })
        | Editing
        | Submitting(None)
        | Submitted => ReasonReact.NoUpdate
        }

      | DismissSubmissionError =>
        switch (state.status) {
        | Editing
        | Submitting(_)
        | Submitted => ReasonReact.NoUpdate
        | SubmissionFailed(_) =>
          ReasonReact.Update({...state, status: FormStatus.Editing})
        }

      | DismissSubmissionResult =>
        switch (state.status) {
        | Editing
        | Submitting(_) => ReasonReact.NoUpdate
        | Submitted
        | SubmissionFailed(_) =>
          ReasonReact.Update({...state, status: FormStatus.Editing})
        }

      | Reset => ReasonReact.Update(initialState->getInitialState)
      },

    render: ({state, send}) =>
      children({
        state: state.input,
        status: state.status,
        result: field =>
          switch (state.fields->Map.get(field)) {
          | None
          | Some(Pristine)
          | Some(Dirty(_, Hidden)) => None
          | Some(Dirty(result, Shown)) => Some(result)
          },
        dirty: () =>
          state.fields
          ->Map.some((_, status) =>
              switch (status) {
              | Dirty(_) => true
              | Pristine => false
              }
            ),
        valid: () =>
          state.fields
          ->Map.every((field, status) =>
              switch (status) {
              | Dirty(Ok(_), _) => true
              | Dirty(Error(_), _) => false
              | Pristine =>
                (state.validators^)
                ->Map.get(field)
                ->Option.map(validator =>
                    switch (state.input->(validator.validate)) {
                    | Ok(_) => true
                    | Error(_) => false
                    }
                  )
                ->Option.getWithDefault(true)
              }
            ),
        submitting:
          switch (state.status) {
          | Submitting(_) => true
          | Editing
          | Submitted
          | SubmissionFailed(_) => false
          },
        change: (field, state) => Change(field, state)->send,
        blur: field => Blur(field)->send,
        submit: () => Submit->send,
        mapSubmissionError: map => MapSubmissionError(map)->send,
        dismissSubmissionError: () => DismissSubmissionError->send,
        dismissSubmissionResult: () => DismissSubmissionResult->send,
        reset: () => Reset->send,
      }),
  };
};
