import React from 'react';
import {
  PaymentCategoryListDocument,
  PaymentItemDocument,
} from '@app/graphql/Payment';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { TextFieldElement } from '@app/ui/fields/text';
import { PersonListDocument } from '@app/graphql/Person';
import { useQuery } from 'urql';
import { ErrorPage } from './ErrorPage';
import { TitleBar } from './TitleBar';
import { z } from 'zod';

const Form = z.object({
  piAmount: z.number(),
  piDate: z.date(),
  piIdCategory: z.string(),
  piIdUser: z.string(),
  piPrefix: z.number().nullish(),
});
type FormProps = z.infer<typeof Form>;

export const PaymentItemForm = ({ id = '' }: {id?: string}) => {
  const [query] = useQuery({query: PaymentItemDocument, variables: { id }, pause: !id});
  const data = query.data?.platbyItem;
  const title = id ? 'Platba' : 'Nová platba';

  const [{ data: users }] = useQuery({query: PersonListDocument});
  const [{ data: categories }] = useQuery({query: PaymentCategoryListDocument});

  // load also platby_raw linked to this one
  // php-unserialize-js the blob
  // on delete, mark raw as !sorted and discarded

  const { reset, control } = useForm<FormProps>();
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  if (query.data && query.data.platbyItem === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2">
      <TitleBar title={title} />

      <fieldset disabled>
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
        placeholder="vyberte uživatele"
        options={(users?.filteredPeopleList || []).map((x) => ({
          id: x.id,
          label: `${x.legacyUserId!.padStart(6, '0')} - ${x.firstName} ${x.lastName}`,
        }))}
      />
      <ComboboxElement
        control={control}
        name="piIdCategory"
        label="Kategorie"
        placeholder="vyberte kategorii"
        options={(categories?.platbyCategories?.nodes || []).map((x) => ({
          id: x.id,
          label: `${x.id} - ${x.pcName}`,
        }))}
      />
      <TextFieldElement control={control} name="piPrefix" label="Prefix (rok)" />
      </fieldset>
    </form>
  );
};
