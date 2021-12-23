import * as React from 'react';
import { FORM_ERROR } from 'final-form'
import { Form, Field } from 'react-final-form'
import { useHistory, useLocation } from 'react-router';
import { useAuth } from '../use-auth';

function AuthButton() {
  const { user, signOut } = useAuth();
  const history = useHistory()
  return !!user
    ? <p>
      Welcome! <button onClick={async () => {
        await signOut();
        history.push('/');
      }}>Sign out</button>
    </p>
    : <p>You are not logged in.</p>
}

export const LoginPage = ({ }) => {
  const { signIn } = useAuth();
  const history = useHistory()
  const { state } = useLocation<{ from?: string }>();

  const onSubmit = async (values: { login: string; password: string; }) => {
    try {
      await signIn(values.login, values.password); // EITHER EMAIl OR USERNAME!!!
      history.push(state?.from || '/dashboard');
      return undefined;
    } catch (e) {
      console.log(e);
      if (values.login !== 'erikras') {
        return { login: 'Unknown username' };
      }
      return { [FORM_ERROR]: 'Login Failed' };
    }
  };
  const validate = (values: { login: string; password: string; }) => ({
    ...(!values.login ? { login: 'Required' } : {}),
    ...(!values.password ? { password: 'Required' } : {}),
  });

  return (
    <Form onSubmit={onSubmit} validate={validate} render={form => (
      <form onSubmit={form.handleSubmit}>
        <Field name="login">
          {({ input, meta }) => <div>
            <label>Login</label>
            <input {...input} type="text" placeholder="Login" />
            {(meta.error || meta.submitError) && meta.touched && (
              <span>{meta.error || meta.submitError}</span>
            )}
          </div>}
        </Field>
        <Field name="password">
          {({ input, meta }) => <div>
            <label>Password</label>
            <input {...input} type="password" placeholder="Password" />
            {meta.error && meta.touched && <span>{meta.error}</span>}
          </div>}
        </Field>
        {form.submitError && <div className="error">{form.submitError}</div>}
        <div className="buttons">
          <button type="submit" disabled={form.submitting}>Log In</button>
        </div>
        <a href="/registrace">Registrovat se</a>
        <a href="/nopassword">Zapomněli jste heslo?</a>
      </form>
    )} />
  );
};
