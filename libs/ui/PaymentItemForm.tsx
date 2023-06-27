import React from 'react';
import {
  CreatePaymentItemDocument,
  DeletePaymentItemDocument,
  PaymentCategoryListDocument,
  PaymentItemDocument,
  UpdatePaymentItemDocument,
} from '@app/graphql/Payment';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { TextFieldElement } from '@app/ui/fields/text';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { UserListDocument } from '@app/graphql/User';
import { useMutation, useQuery } from 'urql';
import { useRouter } from 'next/router';
import { toast } from 'react-toastify';
import { ErrorPage } from './ErrorPage';
import { DeleteButton } from './DeleteButton';
import { PaymentItem } from 'lib/entities';
import { TitleBar } from './TitleBar';
import { z } from 'zod';

const Form = z.object({
  piAmount: z.number(),
  piDate: z.date(),
  piIdCategory: z.string(),
  piIdUser: z.string(),
  piPrefix: z.number().optional(),
});
type FormProps = z.infer<typeof Form>;

const entity = PaymentItem;

export const PaymentItemForm = ({id = ''}: {id?: string}) => {
  const router = useRouter();
  const [query] = useQuery({query: PaymentItemDocument, variables: { id }, pause: !id});
  const data = query.data?.platbyItem;
  const title = id ? 'Platba' : 'Nová platba';

  const create = useMutation(CreatePaymentItemDocument)[1];
  const update = useMutation(UpdatePaymentItemDocument)[1];

  const [{ data: users }] = useQuery({query: UserListDocument});
  const [{ data: categories }] = useQuery({query: PaymentCategoryListDocument});

  // load also platby_raw linked to this one
  // php-unserialize-js the blob
  // on delete, mark raw as !sorted and discarded

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = { ...values, piDate: values.piDate.toString() };
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createPlatbyItem?.platbyItem?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace(entity.editRoute(id));
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.platbyItem === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={entity.listRoute} title={title}>
        {id && (
          <DeleteButton
            doc={DeletePaymentItemDocument}
            id={id}
            title="smazat platbu"
            redirect={entity.listRoute}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <SubmitButton loading={onSubmit.loading} />

      <FormError error={onSubmit.error} />
      <TextFieldElement
        control={control}
        type="date"
        label="Datum"
        name="piDate"
        required
      />
      <TextFieldElement control={control} name="piAmount" label="Částka (Kč)" required />
      <ComboboxElement
        control={control}
        name="piIdUser"
        label="Uživatel"
        options={(users?.users?.nodes || []).map((x) => ({
          id: x.id,
          label: `${x.id.padStart(6, '0')} - ${x.uJmeno} ${x.uPrijmeni}`,
        }))}
      />
      <ComboboxElement
        control={control}
        name="piIdUser"
        label="Uživatel"
        options={(categories?.platbyCategories?.nodes || []).map((x) => ({
          id: x.id,
          label: `${x.id} - ${x.pcName}`,
        }))}
      />
      <TextFieldElement control={control} name="piPrefix" label="Prefix (rok)" />
    </form>
  );
};
