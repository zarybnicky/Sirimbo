import { ReservationDocument } from '@app/graphql/Reservation';
import React from 'react';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from '@app/ui/fields/checkbox';
import { DateRangeInput } from '@app/ui/fields/date';
import { TrainerListDocument } from '@app/graphql/User';
import { ErrorPage } from './ErrorPage';
import { useQuery } from 'urql';
import { TitleBar } from './TitleBar';
import { AdminEntity } from './generic/AdminEntityList';

/* type FormProps = Pick<
*   NabidkaInput,
*   'nTrener' | 'nPocetHod' | 'nMaxPocetHod' | 'nVisible' | 'nLock'
* > & {
*   schedule: DateRange;
* }; */

export const ReservationForm = ({ entity, id = '' }: { entity: AdminEntity; id?: string }) => {
  const [query] = useQuery({query: ReservationDocument, variables: { id }, pause: !id });
  const data = query.data?.nabidka;
  const title = id ? data?.userByNTrener?.fullName || '(Bez názvu)' : 'Nová nabídka';

  const [{ data: trainers }] = useQuery({query: TrainerListDocument});

  const { reset, control } = useForm();
  React.useEffect(() => {
    reset({
      nTrener: data?.nTrener,
      nPocetHod: data?.nPocetHod,
      nMaxPocetHod: data?.nMaxPocetHod,
      nVisible: data?.nVisible,
      nLock: data?.nLock,
      schedule: {
        from: data?.nOd ? new Date(data?.nOd) : undefined,
        to: data?.nDo ? new Date(data?.nDo) : undefined,
      },
    });
  }, [reset, data]);

  if (query.data && query.data.nabidka === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2">
      <TitleBar backHref={entity.listRoute} title={title} />

      <ComboboxElement
        control={control}
        name="nTrener"
        label="Trenér"
        placeholder="žádný trenér"
        options={(trainers?.trainers?.nodes || []).map((x) => ({
          id: x.id,
          label: `${x.uJmeno} ${x.uPrijmeni}`,
        }))}
      />
      <TextFieldElement control={control} name="nPocetHod" label="Počet hodin" required />
      <TextFieldElement
        control={control}
        name="nMaxPocetHod"
        label="Max.počet hodin"
        required
      />
      <DateRangeInput control={control} name="schedule" label="Datum" />
      <CheckboxElement control={control} name="nVisible" value="1" label="Viditelný" />
      <CheckboxElement control={control} name="nLock" value="1" label="Uzamčený" />
    </form>
  );
};
