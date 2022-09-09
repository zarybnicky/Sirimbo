import * as React from 'react';
import { Card, CardContent, CardActions, Container, Grid, Typography, Button } from '@mui/material';
import { Alert } from '@mui/lab';
import { FORM_ERROR } from 'final-form'
import { Form } from 'react-final-form'
import Link from 'next/link';
import { TextField } from 'mui-rff';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';

export const LoginPage = ({ }) => {
  const { signIn } = useAuth();
  const router = useRouter()

  const onSubmit = async (values: { login: string; passwd: string; }) => {
    try {
      await signIn(values.login, values.passwd);
      router.push(router.query?.from as string || '/dashboard');
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
          <TextField label="E-mail nebo přihlašovací jméno" name="login" autoComplete="username" required autoFocus />
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
        <Link href="/register" passHref><Button size="small">Registrovat se</Button></Link>
        <Link href="/forgotten-password"><Button size="small">Zapomněli jste heslo?</Button></Link>
      </Grid>
    </React.Fragment>} />
  </Container>
};

export default LoginPage;
