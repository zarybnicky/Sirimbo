import * as React from 'react';
import { Card, CardContent, CardActions, Container, Grid, Typography, Button } from '@mui/material';
import { Alert } from '@mui/lab';
import Link from 'next/link';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';

type FormProps = {
  login: string;
  passwd: string;
};

export const LoginPage = ({ }) => {
  const { signIn } = useAuth();
  const router = useRouter()
  const [submitError, setSubmitError] = React.useState<string | null>(null);
  const methods = useForm<FormProps>();
  const { handleSubmit, formState: { isDirty, isValid, isSubmitting } } = methods;

  const onSubmit = async (values: FormProps) => {
    setSubmitError(null);
    try {
      await signIn(values.login, values.passwd);
      router.push(router.query?.from as string || '/dashboard');
      return undefined;
    } catch (e) {
      setSubmitError(
        e instanceof Error ? (
          e.message === 'ACCOUNT_NOT_FOUND' ? 'Účet nenalezen' :
            e.message === 'INVALID_PASSWORD' ? 'Nesprávné heslo' :
              e.message === 'ACCOUNT_DISABLED' ? 'Učet byl zablokován' :
                e.message === 'ACCOUNT_NOT_CONFIRMED' ? 'Účet ještě nebyl potvrzen' :
                  'Neznámá chyba'
        ) : 'Neznámá chyba',
      );
    }
  };

  return <Container maxWidth="xs" style={{ margin: '4rem auto 6rem' }}>
    <Card component="form" onSubmit={handleSubmit(onSubmit)}>
      <CardContent>
        <Typography gutterBottom variant="h5" component="h2">Přihlášení do systému</Typography>
        {submitError && <Alert severity="error">{submitError}</Alert>}
        <TextFieldElement label="E-mail nebo přihlašovací jméno" name="login" autoComplete="username" required autoFocus />
        <TextFieldElement label="Heslo" name="passwd" type="password" autoComplete="current-password" required />
      </CardContent>
      <CardActions style={{ flexDirection: 'column' }}>
        <Button
          fullWidth variant="contained" type="submit" color="primary"
          disabled={isValid && !isSubmitting && isDirty}
        >Přihlásit</Button>
      </CardActions>
    </Card>
    <Grid container justifyContent="space-between" style={{ marginTop: '1rem' }}>
      <Link href="/register" passHref><Button size="small">Registrovat se</Button></Link>
      <Link href="/forgotten-password"><Button size="small">Zapomněli jste heslo?</Button></Link>
    </Grid>
  </Container>
};

export default LoginPage;
