import * as React from 'react';
import { Alert, Card, CardContent, CardActions, Grid, Typography, Button } from '@mui/material';
import { NextLinkComposed } from 'components/Link';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';

type FormProps = {
  login: string;
  passwd: string;
};

export const LoginForm: React.FC<{
  onSuccess?: () => void;
}> = ({ onSuccess }) => {
  const { signIn } = useAuth();
  const router = useRouter()
  const [submitError, setSubmitError] = React.useState<string | null>(null);
  const { control, handleSubmit, formState: { isDirty, isValid, isSubmitting } } = useForm<FormProps>();

  const onSubmit = async (values: FormProps) => {
    setSubmitError(null);
    try {
      await signIn(values.login, values.passwd);
      onSuccess && onSuccess();
      router.push(router.query?.from as string || '/dashboard');
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

  return <>
    <Card component="form" onSubmit={handleSubmit(onSubmit)}>
      <CardContent style={{ gap: '5px', display: 'flex', flexDirection: 'column', justifyItems: 'stretch' }}>
        <Typography gutterBottom variant="h5" component="h2">
          Přihlášení do systému
        </Typography>
        {submitError && <Alert severity="error">{submitError}</Alert>}
        <TextFieldElement
          control={control} name="login"
          label="E-mail nebo přihlašovací jméno" autoComplete="username"
          required autoFocus
        />
        <TextFieldElement
          control={control} name="passwd" type="password"
          label="Heslo" autoComplete="current-password" required
        />
      </CardContent>
      <CardActions style={{ flexDirection: 'column', padding: '0 1rem 1rem' }}>
        <Button
          fullWidth variant="contained" type="submit" color="primary"
          disabled={isValid && !isSubmitting && isDirty}
        >Přihlásit</Button>
      </CardActions>
      <Grid container justifyContent="space-between" style={{ padding: '0 1rem 1rem' }}>
        <Button component={NextLinkComposed} href="/register" size="small">Registrovat se</Button>
        <Button component={NextLinkComposed} href="/forgotten-password" size="small">Zapomněli jste heslo?</Button>
      </Grid>
    </Card>
  </>;
};
