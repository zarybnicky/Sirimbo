import { LoginDocument, type UserAuthFragment } from '@/graphql/CurrentUser';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import Link from 'next/link';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useTenantConfig } from '../state/auth';

const Form = z.object({
  login: z.string().min(1, 'Zadejte přihlašovací jméno nebo e-mail'),
  passwd: z.string().min(1, 'Zadejte heslo'),
});

export function LoginForm({
  onSuccess,
}: {
  onSuccess?: (result: UserAuthFragment | null) => void;
}) {
  const [result, executeLogin] = useMutation(LoginDocument);
  const { enableRegistration } = useTenantConfig();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });

  const onSubmit = async ({ login, passwd }: z.infer<typeof Form>) => {
    const result = await executeLogin({ login, passwd });
    if (!result.error) onSuccess?.(result.data?.login?.result?.usr ?? null);
  };

  return (
    <div className="flex h-[calc(100dvh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
      <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit)}>
          <h4 className="text-2xl">Přihlášení do systému</h4>

          <FormError error={result.error} />
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
