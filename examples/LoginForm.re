module LoginForm = {
  open Formality;

  type field =
    | Email
    | Password
    | RememberMe;

  type state = {
    email: string,
    password: string,
    rememberMe: bool,
  };

  type message = string;
  type submissionError = unit;

  module EmailField = {
    let update = (value, state) => {...state, email: value};

    let validator = {
      field: Email,
      strategy: Strategy.OnFirstSuccessOrFirstBlur,
      dependents: None,
      validate: ({email}) => {
        let emailRegex = [%bs.re {|/.*@.*\..+/|}];
        switch (email) {
        | "" => Error("Email is required")
        | _ as value when !emailRegex->Js.Re.test_(value) =>
          Error("Email is invalid")
        | _ => Ok(Valid)
        };
      },
    };
  };

  module PasswordField = {
    let update = (value, state) => {...state, password: value};

    let validator = {
      field: Password,
      strategy: Strategy.OnFirstBlur,
      dependents: None,
      validate: ({password}) =>
        switch (password) {
        | "" => Error("Password is required")
        | _ => Ok(Valid)
        },
    };
  };

  module RememberMeField = {
    let update = (value, state) => {...state, rememberMe: value};
  };

  let validators = [EmailField.validator, PasswordField.validator];
};

module LoginFormHook = Formality.Make(LoginForm);

let initialState = LoginForm.{email: "", password: "", rememberMe: false};

[@react.component]
let make = () => {
  let form =
    LoginFormHook.useForm(
      ~initialState,
      ~initialValidators=LoginForm.validators,
      ~onSubmit=(state, form) => {
        Js.log2("Submitted with:", state);
        Js.Global.setTimeout(
          () => {
            form.notifyOnSuccess(None);
            form.reset->Js.Global.setTimeout(3000)->ignore;
          },
          500,
        )
        ->ignore;
      },
    );

  <form className="form" onSubmit={form.submit->Formality.Dom.preventDefault}>
    <div className="form-messages-area form-messages-area-lg" />
    <div className="form-content">
      <h2 className="push-lg"> "Login"->React.string </h2>
      <div className="form-row">
        <label htmlFor="login--email" className="label-lg">
          "Email"->React.string
        </label>
        <input
          id="login--email"
          type_="text"
          value={form.state.email}
          disabled={form.submitting}
          onBlur={_ => form.blur(Email)}
          onChange={event =>
            form.change(
              ~field=Email,
              LoginForm.EmailField.update(
                event->ReactEvent.Form.target##value,
              ),
            )
          }
        />
        {switch (Email->(form.result)) {
         | Some(Error(message)) =>
           <div className={Cn.make(["form-message", "failure"])}>
             message->React.string
           </div>
         | Some(Ok(Valid)) =>
           <div className={Cn.make(["form-message", "success"])}>
             {j|✓|j}->React.string
           </div>
         | Some(Ok(NoValue))
         | None => React.null
         }}
      </div>
      <div className="form-row">
        <label htmlFor="login--password" className="label-lg">
          "Password"->React.string
        </label>
        <input
          id="login--password"
          type_="text"
          value={form.state.password}
          disabled={form.submitting}
          onBlur={_ => form.blur(Password)}
          onChange={event =>
            form.change(
              ~field=Password,
              LoginForm.PasswordField.update(
                event->ReactEvent.Form.target##value,
              ),
            )
          }
        />
        {switch (Password->(form.result)) {
         | Some(Error(message)) =>
           <div className={Cn.make(["form-message", "failure"])}>
             message->React.string
           </div>
         | Some(Ok(Valid)) =>
           <div className={Cn.make(["form-message", "success"])}>
             {j|✓|j}->React.string
           </div>
         | Some(Ok(NoValue))
         | None => React.null
         }}
      </div>
      <div className="form-row">
        <input
          id="login--remember"
          type_="checkbox"
          checked={form.state.rememberMe}
          disabled={form.submitting}
          className="push-lg"
          onBlur={_ => form.blur(RememberMe)}
          onChange={event =>
            form.change(
              ~field=RememberMe,
              LoginForm.RememberMeField.update(
                event->ReactEvent.Form.target##checked,
              ),
            )
          }
        />
        <label htmlFor="login--remember"> "Remember me"->React.string </label>
      </div>
      <div className="form-row">
        <button className="push-lg" disabled={form.submitting}>
          (form.submitting ? "Submitting..." : "Submit")->React.string
        </button>
        {switch (form.status) {
         | Submitted =>
           <div className={Cn.make(["form-status", "success"])}>
             {j|✓ Logged In|j}->React.string
           </div>
         | _ => React.null
         }}
      </div>
    </div>
  </form>;
};
