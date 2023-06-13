import React from 'react';
import {
  CreatePaymentItemDocument,
  DeletePaymentItemDocument,
  PaymentCategoryListDocument,
  PaymentItemDocument,
  UpdatePaymentItemDocument,
} from '@app/graphql/Payment';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from 'components/Combobox';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { PlatbyItemInput } from '@app/graphql';
import { UserListDocument } from '@app/graphql/User';
import { useMutation, useQuery } from 'urql';
import { useRouter } from 'next/router';
import { toast } from 'react-toastify';
import { ErrorPage } from './ErrorPage';
import { DeleteButton } from './DeleteButton';
import { PaymentItem } from 'lib/entities';
import { TitleBar } from './layout/TitleBar';

type FormProps = Pick<
  PlatbyItemInput,
  'piAmount' | 'piDate' | 'piIdCategory' | 'piIdUser' | 'piPrefix'
>;
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
    reset({
      piAmount: data?.piAmount,
      piDate: data?.piDate,
      piIdCategory: data?.piIdCategory,
      piIdUser: data?.piIdUser,
      piPrefix: data?.piPrefix,
    });
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
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
            onDelete={() => router.push(entity.listRoute)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <SubmitButton loading={onSubmit.loading} />

      <ErrorBox error={onSubmit.error} />
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
        required
        options={(users?.users?.nodes || []).map((x) => ({
          id: x.id,
          label: `${x.id.padStart(6, '0')} - ${x.uJmeno} ${x.uPrijmeni}`,
        }))}
      />
      <ComboboxElement
        control={control}
        name="piIdUser"
        label="Uživatel"
        required
        options={(categories?.platbyCategories?.nodes || []).map((x) => ({
          id: x.id,
          label: `${x.id} - ${x.pcName}`,
        }))}
      />
      <TextFieldElement control={control} name="piPrefix" label="Prefix (rok)" required />
    </form>
  );
};
