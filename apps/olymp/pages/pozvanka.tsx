import React from 'react';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from 'components/layout/Layout';
import { StringParam, useQueryParam, withDefault } from 'use-query-params';
import { SubmitButton } from '@app/ui/submit';
import { TextFieldElement } from '@app/ui/fields/text';
import { FormError } from '@app/ui/form';
import { useZodForm } from 'lib/use-schema-form';
import { TypeOf, z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { InvitationInfoDocument, RegisterUsingInvitationDocument } from '@app/graphql/CurrentUser';
import { useRouter } from 'next/router';

const Form = z.object({
    login: z.string(),
    email: z.string().email(),
    passwd: z.string(),
    token: z.string(),
});

function InvitationPage() {
  const router = useRouter();
  const [token] = useQueryParam('token', withDefault(StringParam, ''));
  const { reset, control, handleSubmit } = useZodForm(Form);

  const [{ data, fetching }] = useQuery({ query: InvitationInfoDocument, variables: { token }, pause: !token });
  const register = useMutation(RegisterUsingInvitationDocument)[1];

  React.useEffect(() => {
    reset({token, email: data?.invitationInfo || ''});
  }, [data]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await register({ input: values });
    router.replace('/dashboard');
  });

  return (
    <Layout className="content-stretch">
      <TitleBar title="Registrace" />

    <div className="flex items-center justify-center h-full">
      <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4 className="text-2xl">Registrace nového uživatele</h4>

          <FormError error={onSubmit.error} />
          {!fetching && !data?.invitationInfo && <FormError error="Vaše pozvánka je neplatná nebo již použitá." />}
          <TextFieldElement
            control={control}
            name="login"
            label="E-mail nebo přihlašovací jméno"
            autoComplete="username"
            required
            autoFocus
          />
          <TextFieldElement
            control={control}
            name="passwd"
            type="password"
            label="Heslo"
            autoComplete="current-password"
            required
          />
          <SubmitButton className="w-full my-2" loading={onSubmit.loading}>
            Přihlásit
          </SubmitButton>
        </form>
      </div>
    </div>
    </Layout>
  );
};

export default InvitationPage;
