import {
  CreatePaymentGroupDocument,
  DeletePaymentGroupDocument,
  PaymentGroupDocument,
  UpdatePaymentGroupDocument,
} from '@app/graphql/Payment';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { TextAreaElement } from '@app/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { PlatbyGroupInput } from '@app/graphql';
import { useMutation, useQuery } from 'urql';
import { useRouter } from 'next/router';
import { ErrorPage } from './ErrorPage';
import { toast } from 'react-toastify';
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
import { TitleBar } from './TitleBar';

type FormProps = Pick<PlatbyGroupInput, 'pgName' | 'pgDescription' | 'pgBase'>;

const backHref: Route = { pathname: '/admin/platby/structure/group' };

export const PaymentGroupForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({
    query: PaymentGroupDocument,
    variables: { id },
    pause: !id,
  });
  const data = query.data?.platbyGroup;
  const title = id ? data?.pgName || '(Bez názvu)' : 'Nová skupina';

  const create = useMutation(CreatePaymentGroupDocument)[1];
  const update = useMutation(UpdatePaymentGroupDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      pgName: data?.pgName,
      pgDescription: data?.pgDescription,
      pgBase: data?.pgBase,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (id) {
      await update({ id, patch: values });
    } else {
      const res = await create({ input: values });
      const id = res.data?.createPlatbyGroup?.platbyGroup?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/rozpis/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.platbyGroup === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={backHref} title={title}>
        {id && (
          <DeleteButton
            doc={DeletePaymentGroupDocument}
            id={id}
            title="smazat kategorii"
            redirect={backHref}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="pgName" label="Název" required />
      <TextAreaElement
        control={control}
        name="pgDescription"
        label="Shrnutí"
        rows={3}
        required
      />
      <TextFieldElement
        control={control}
        type="number"
        name="pgBase"
        label="Násobitel částky"
        required
      />
    </form>
  );
};
