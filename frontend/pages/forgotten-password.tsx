import { Container, Typography, Grid, Card, CardContent, CardActions, Button, Alert } from "@mui/material";
import { useResetPasswordMutation } from 'lib/graphql';
import { useRequireUserLoggedOut } from "lib/route-guards";
import { useRouter } from "next/router";
import { useSnackbar } from "notistack";
import React from "react";
import { useForm } from "react-hook-form";
import { TextFieldElement } from "react-hook-form-mui";

export default function ForgottenPassword() {
  useRequireUserLoggedOut();
  const { control, handleSubmit, formState: { isDirty, isSubmitting } } = useForm();
  const { enqueueSnackbar } = useSnackbar();
  const [submitError, setSubmitError] = React.useState<string | null>(null);
  const { mutateAsync: resetPassword } = useResetPasswordMutation();
  const router = useRouter();

  const onSubmit = async (data: any, event: any) => {
    if (event) event.preventDefault();
    setSubmitError(null);
    try {
      await resetPassword({ input: data });
      enqueueSnackbar('Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu', { variant: 'success' });
      router.push('/');
    } catch (e) {
      if (e instanceof Error) {
        setSubmitError(e.message);
      } else {
        setSubmitError('Nepodařilo se změnit heslo, prosím kontaktujte administrátora.');
      }
    }
  };

  return <Container maxWidth="xs" style={{ margin: '4rem auto 6rem' }}>
    <Card component="form" onSubmit={handleSubmit(onSubmit)}>
      <CardContent>
        <Typography gutterBottom variant="h5" component="h2">
          Zapomenuté heslo
        </Typography>
        <Typography variant="body1" style={{ marginBottom: '1rem' }}>
          Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který jste zadali při registraci.
        </Typography>

        <Grid container spacing={3}>
          <Grid item xs={12}>
            <TextFieldElement fullWidth control={control} name="login" label="Přihlašovací jméno" autoComplete="login" required />
          </Grid>
          <Grid item xs={12}>
            <TextFieldElement fullWidth control={control} type="email" name="email" label="E-mail" autoComplete="email" required />
          </Grid>
          {submitError === 'ACCOUNT_NOT_FOUND' && (
            <Grid item xs={12}>
              <Alert severity="error">Zadaná kombinace jména a e-mailu neexistuje</Alert>
            </Grid>
          )}
        </Grid>
      </CardContent>
      <CardActions>
        <Button
          fullWidth variant="contained" type="submit" color="primary"
          disabled={isSubmitting || !isDirty}
        >Resetovat heslo</Button>
      </CardActions>
    </Card>
  </Container>;
}
