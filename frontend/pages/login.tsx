import * as React from 'react';
import { Card, CardContent, CardActions, Container, Grid, Typography, Button } from '@mui/material';
import { NextLinkComposed } from 'components/Link';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { useRequireUserLoggedOut } from 'lib/route-guards';
import { ErrorBox } from 'components/ErrorBox';
import { useAsyncCallback } from 'react-async-hook';

type FormProps = {
  login: string;
  passwd: string;
};

export default function LoginPage() {
  useRequireUserLoggedOut();
  const { signIn } = useAuth();
  const router = useRouter()
  const { control, handleSubmit, formState: { isDirty, isValid, isSubmitting } } = useForm<FormProps>();

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await signIn(values.login, values.passwd);
    router.push(router.query?.from as string || '/dashboard');
  });

  return (
    <Container maxWidth="xs" style={{ margin: '4rem auto 6rem' }}>
      <Card component="form" onSubmit={handleSubmit(onSubmit.execute)}>
        <CardContent style={{ gap: '5px', display: 'flex', flexDirection: 'column', justifyItems: 'stretch' }}>
          <Typography gutterBottom variant="h5" component="h2">
            Přihlášení do systému
          </Typography>
          <ErrorBox grid error={onSubmit.error} />
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
    </Container>
  );
};
