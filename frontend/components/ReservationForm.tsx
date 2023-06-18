import {
  CreateReservationDocument,
  DeleteReservationDocument,
  ReservationDocument,
  UpdateReservationDocument,
} from '@app/graphql/Reservation';
import React from 'react';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from 'components/Combobox';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { NabidkaInput } from '@app/graphql';
import { DateRange, DateRangeInput } from '@app/ui/fields/date';
import { TrainerListDocument } from '@app/graphql/User';
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
import { useRouter } from 'next/router';
import { toast } from 'react-toastify';
import { ErrorPage } from './ErrorPage';
import { useMutation, useQuery } from 'urql';
import { TitleBar } from './layout/TitleBar';

type FormProps = Pick<
  NabidkaInput,
  'nTrener' | 'nPocetHod' | 'nMaxPocetHod' | 'nVisible' | 'nLock'
> & {
  schedule: DateRange;
};

const backHref: Route = { pathname: '/admin/nabidka' };

export const ReservationForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({query: ReservationDocument, variables: { id }, pause: !id });
  const data = query.data?.nabidka;
  const title = id ? data?.userByNTrener?.fullName || '(Bez názvu)' : 'Nová nabídka';

  const create = useMutation(CreateReservationDocument)[1];
  const update = useMutation(UpdateReservationDocument)[1];
  const [{ data: trainers }] = useQuery({query: TrainerListDocument});

  const { reset, control, handleSubmit } = useForm<FormProps>();
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

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      nTrener: values.nTrener,
      nPocetHod: values.nPocetHod,
      nMaxPocetHod: values.nMaxPocetHod,
      nVisible: values.nVisible,
      nLock: values.nLock,
      nOd: values.schedule.from?.toISOString() || '',
      nDo: values.schedule.to?.toDateString() || '',
    };
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data!.createNabidka?.nabidka?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/nabidka/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.nabidka === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={backHref} title={title}>
        {id && (
          <DeleteButton
            doc={DeleteReservationDocument}
            id={id}
            title="smazat nabídku"
            onDelete={() => router.push(backHref)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="nTrener"
        label="Trenér"
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
