import { Card, CardContent, CardActions } from "@mui/material";
import { ErrorBox } from "components/ErrorBox";
import { SubmitButton } from "components/SubmitButton";
import { useResetPasswordMutation } from 'lib/graphql';
import { useRequireUserLoggedOut } from "lib/route-guards";
import { useRouter } from "next/router";
import { useSnackbar } from "notistack";
import React from "react";
import { useAsyncCallback } from "react-async-hook";
import { useForm } from "react-hook-form";
import { TextFieldElement } from "components/TextField";

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

  return <div className="container mx-auto max-w-md" style={{ margin: '4rem auto 6rem' }}>
    <Card component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <CardContent>
        <h5>Zapomenuté heslo</h5>
        <div className="mb-4">
          Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který jste zadali při registraci.
        </div>

        <div className="flex flex-col gap-4">
          <TextFieldElement control={control} name="login" label="Přihlašovací jméno" autoComplete="login" required />
          <TextFieldElement control={control} type="email" name="email" label="E-mail" autoComplete="email" required />
          <ErrorBox error={onSubmit.error} default="Nepodařilo se změnit heslo, prosím kontaktujte administrátora." />
        </div>
      </CardContent>
      <CardActions>
        <SubmitButton className="w-full" loading={onSubmit.loading} disabled={!formState.isValid}>
          Obnovit heslo
        </SubmitButton>
      </CardActions>
    </Card>
  </div>;
}
