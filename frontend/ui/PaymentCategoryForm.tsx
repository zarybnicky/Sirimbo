import {PaymentCategoryDocument,} from '@app/graphql/Payment';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from '@app/ui/fields/checkbox';
import { useQuery } from 'urql';
import { ErrorPage } from './ErrorPage';
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
  pcUsePrefix: z.boolean().nullish(),
  pcArchive: z.boolean().nullish(),
  pcVisible: z.boolean().nullish(),
});
type FormProps = z.infer<typeof Form>;

export const PaymentCategoryForm = ({ id = '' }: { id?: string }) => {
  const [query] = useQuery({query: PaymentCategoryDocument, variables: { id }, pause: !id});
  const data = query.data?.platbyCategory;
  const title = id ? data?.pcName || '(Bez názvu)' : 'Nová platba';

  const { reset, control } = useForm<FormProps>({ resolver: zodResolver(Form) });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [data, reset]);

  if (query.data && query.data.platbyCategory === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2">
      <TitleBar title={title} />

      <fieldset disabled>
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
      </fieldset>
    </form>
  );
};
