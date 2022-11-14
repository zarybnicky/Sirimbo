import * as React from 'react';
import { Card, CardContent, CardActions, Grid, Typography } from '@mui/material';
import Link from 'next/link';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useRequireUserLoggedOut } from 'lib/route-guards';
import { ErrorBox } from 'components/ErrorBox';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from 'components/SubmitButton';

type FormProps = {
  login: string;
  passwd: string;
};

export default function LoginPage() {
  useRequireUserLoggedOut();
  const { signIn } = useAuth();
  const router = useRouter();
  const { control, handleSubmit, formState } = useForm<FormProps>();

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await signIn(values.login, values.passwd);
    router.push(router.query?.from as string || '/dashboard');
  });

  return (
    <div className="container mx-auto max-w-md mt-12 mb-16">
      <Card component="form" onSubmit={handleSubmit(onSubmit.execute)}>
        <CardContent style={{ gap: '5px', display: 'flex', flexDirection: 'column', justifyItems: 'stretch' }}>
          <Typography gutterBottom variant="h5" component="h2">
            Přihlášení do systému
          </Typography>
          <ErrorBox error={onSubmit.error} />
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
          <SubmitButton className="w-full" loading={onSubmit.loading} disabled={!formState.isValid}>
            Přihlásit
          </SubmitButton>
        </CardActions>
        <Grid container justifyContent="space-between" style={{ padding: '0 1rem 1rem' }}>
          <Link href="/register" passHref>
            <a className="button button-red button-sm button-text flex gap-2 items-center">
              Registrovat se
            </a>
          </Link>
          <Link href="/forgotten-password" passHref>
            <a className="button button-red button-sm button-text flex gap-2 items-center">
              Zapomněli jste heslo?
            </a>
          </Link>
        </Grid>
      </Card>
    </div>
  );
};
