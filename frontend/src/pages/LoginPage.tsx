import * as React from 'react';
import { Card, CardContent, CardActions, Container, Grid, Typography, Button } from '@material-ui/core';
import { Alert } from '@material-ui/lab';
import { FORM_ERROR } from 'final-form'
import { Form } from 'react-final-form'
import { useHistory, useLocation } from 'react-router';
import { Link } from 'react-router-dom';
import { TextField } from 'mui-rff';
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

  const onSubmit = async (values: { login: string; passwd: string; }) => {
    try {
      await signIn(values.login, values.passwd);
      history.push(state?.from || '/dashboard');
      return undefined;
    } catch (e) {
      return {
        [FORM_ERROR]:
          e instanceof Error ? (
            e.message === 'ACCOUNT_NOT_FOUND' ? 'Účet nenalezen' :
              e.message === 'INVALID_PASSWORD' ? 'Nesprávné heslo' :
                e.message === 'ACCOUNT_DISABLED' ? 'Učet byl zablokován' :
                  e.message === 'ACCOUNT_NOT_CONFIRMED' ? 'Účet ještě nebyl potvrzen' :
                    'Neznámá chyba'
          ) : 'Neznámá chyba',
      };
    }
  };

  return <Container maxWidth="xs" style={{ margin: '4rem auto 6rem' }}>
    <Form onSubmit={onSubmit} render={form => <React.Fragment>
      {form.submitError && <Alert severity="error">{form.submitError}</Alert>}
      <Card component="form" onSubmit={form.handleSubmit}>
        <CardContent>
          <Typography gutterBottom variant="h5" component="h2">Přihlášení do systému</Typography>
          <TextField label="E-mail nebo přihlašovací jméno" name="login" autoComplete="username" required />
          <TextField label="Heslo" name="passwd" type="password" autoComplete="current-password" required />
        </CardContent>
        <CardActions style={{ flexDirection: 'column' }}>
          <Button
            fullWidth variant="contained" type="submit" color="primary"
            disabled={form.pristine || form.submitting}
          >Přihlásit</Button>
        </CardActions>
      </Card>
      <Grid container justifyContent="space-between" style={{ marginTop: '1rem' }}>
        <Button component={Link} size="small" to="/registration">Registrovat se</Button>
        <Button component={Link} size="small" to="/forgotten-password">Zapomněli jste heslo?</Button>
      </Grid>
    </React.Fragment>} />
  </Container>
};
