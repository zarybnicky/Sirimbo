'use client';

import type { LoginMutationVariables, UserAuthFragment } from '@/graphql/CurrentUser';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useRedirectLoggedIn } from '@/ui/use-auth';
import { zodResolver } from '@hookform/resolvers/zod';
import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { z } from 'zod';

const Form = z.object({
  login: z.string().min(1, 'Zadejte přihlašovací jméno nebo e-mail'),
  passwd: z.string().min(1, 'Zadejte heslo'),
});

export function LoginForm({
  login,
  enableRegistration,
  from,
  defaultRedirect,
}: {
  login: (
    values: LoginMutationVariables,
  ) => Promise<{ error: string } | { user: UserAuthFragment | null }>;
  enableRegistration: boolean;
  from?: string;
  defaultRedirect: string;
}) {
  const router = useRouter();
  useRedirectLoggedIn();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    const result = await login(values);
    if ('error' in result) throw new Error(result.error);
    router.push(!result.user?.userProxiesList.length ? '/profil' : from || defaultRedirect);
  });

  return (
    <div className="flex h-[calc(100dvh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
      <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4 className="text-2xl">Přihlášení do systému</h4>

          <FormError error={onSubmit.error} />
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
          <SubmitButton control={control} className="my-2">
            Přihlásit
          </SubmitButton>

          <div className="flex flex-wrap mt-2 -mx-3 gap-2 justify-between">
            <div>
              {enableRegistration && (
                <Link
                  href="/registrace"
                  className="uppercase rounded-md px-3 text-sm py-2 text-accent-10 hover:bg-accent-3 text-left"
                >
                  Registrace nového člena
                </Link>
              )}
            </div>
            <div>
              <Link
                href="/zapomenute-heslo"
                className="uppercase rounded-md px-3 text-sm py-2 text-accent-10 hover:bg-accent-3 text-right"
              >
                Zapomněli jste heslo?
              </Link>
            </div>
          </div>
        </form>
      </div>
    </div>
  );
}
