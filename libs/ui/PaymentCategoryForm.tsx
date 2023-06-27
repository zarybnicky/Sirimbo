import {CreatePaymentCategoryDocument, DeletePaymentCategoryDocument, PaymentCategoryDocument, UpdatePaymentCategoryDocument,} from '@app/graphql/Payment';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from '@app/ui/fields/checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useMutation, useQuery } from 'urql';
import { useRouter } from 'next/router';
import { ErrorPage } from './ErrorPage';
import { toast } from 'react-toastify';
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
import { TitleBar } from './TitleBar';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';

const Form = z.object({
  pcName: z.string(),
  pcSymbol: z.string(),
  pcAmount: z.number(),
  pcDateDue: z.string(),
  pcValidFrom: z.string(),
  pcValidTo: z.string(),
  pcUsePrefix: z.boolean().optional(),
  pcArchive: z.boolean().optional(),
  pcVisible: z.boolean().optional(),
});
type FormProps = z.infer<typeof Form>;

const backHref: Route = { pathname: '/admin/platby/structure/category' };

export const PaymentCategoryForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({query: PaymentCategoryDocument, variables: { id }, pause: !id});
  const data = query.data?.platbyCategory;
  const title = id ? data?.pcName || '(Bez názvu)' : 'Nová platba';

  const create = useMutation(CreatePaymentCategoryDocument)[1];
  const update = useMutation(UpdatePaymentCategoryDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createPlatbyCategory?.platbyCategory?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/rozpis/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.platbyCategory === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={backHref} title={title}>
        {id && (
          <DeleteButton
            doc={DeletePaymentCategoryDocument}
            id={id}
            title="smazat rozpis"
            redirect={backHref}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="pcName" label="Název" required />
      <TextFieldElement
        control={control}
        name="pcSymbol"
        label="Specifický symbol"
        required
      />
      <TextFieldElement
        control={control}
        name="pcAmount"
        label="Očekávaná částka"
        type="number"
        required
      />
      <TextFieldElement
        control={control}
        type="date"
        label="Splatnost"
        name="pcDateDue"
        required
      />
      <TextFieldElement
        control={control}
        type="date"
        label="Platné od"
        name="pcValidFrom"
        required
      />
      <TextFieldElement
        control={control}
        type="date"
        label="Platné do"
        name="pcValidTo"
        required
      />
      <CheckboxElement
        control={control}
        name="pcUsePrefix"
        value="1"
        label="Použít prefix"
      />
      <CheckboxElement control={control} name="pcArchive" value="1" label="Archiv" />
      <CheckboxElement control={control} name="pcVisible" value="1" label="Viditelný" />
    </form>
  );
};
