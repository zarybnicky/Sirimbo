import { Container, Typography, Grid, Card, CardContent, CardActions } from "@mui/material";
import { ErrorBox } from "components/ErrorBox";
import { SubmitButton } from "components/SubmitButton";
import { useResetPasswordMutation } from 'lib/graphql';
import { useRequireUserLoggedOut } from "lib/route-guards";
import { useRouter } from "next/router";
import { useSnackbar } from "notistack";
import React from "react";
import { useAsyncCallback } from "react-async-hook";
import { useForm } from "react-hook-form";
import { TextFieldElement } from "react-hook-form-mui";

export default function ForgottenPassword() {
  useRequireUserLoggedOut();
  const { control, handleSubmit, formState } = useForm();
  const { enqueueSnackbar } = useSnackbar();
  const { mutateAsync: resetPassword } = useResetPasswordMutation();
  const router = useRouter();

  const onSubmit = useAsyncCallback(async (data: any) => {
    await resetPassword({ input: data });
    enqueueSnackbar('Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu', { variant: 'success' });
    router.push('/');
  });

  return <Container maxWidth="xs" style={{ margin: '4rem auto 6rem' }}>
    <Card component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <CardContent>
        <Typography gutterBottom variant="h5" component="h2">
          Zapomenuté heslo
        </Typography>
        <Typography variant="body1" style={{ marginBottom: '1rem' }}>
          Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který jste zadali při registraci.
        </Typography>

        <Grid container spacing={1.5}>
          <Grid item xs={12}>
            <TextFieldElement fullWidth control={control} name="login" label="Přihlašovací jméno" autoComplete="login" required />
          </Grid>
          <Grid item xs={12}>
            <TextFieldElement fullWidth control={control} type="email" name="email" label="E-mail" autoComplete="email" required />
          </Grid>
          <ErrorBox grid error={onSubmit.error} default="Nepodařilo se změnit heslo, prosím kontaktujte administrátora." />
        </Grid>
      </CardContent>
      <CardActions>
        <SubmitButton className="w-full" loading={onSubmit.loading} disabled={!formState.isValid}>
          Obnovit heslo
        </SubmitButton>
      </CardActions>
    </Card>
  </Container>;
}
