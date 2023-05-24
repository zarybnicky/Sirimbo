import {
    CreateReservationDocument,
  ReservationFragment, UpdateReservationDocument,
} from 'lib/graphql/Reservation';
import React from 'react';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from 'components/Combobox';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { NabidkaInput } from 'lib/graphql';
import { DateRange, DateRangeInput } from './DateRange';
import { useGqlMutation, useGqlQuery } from 'lib/query';
import { TrainerListDocument } from 'lib/graphql/User';

type FormProps = Pick<
  NabidkaInput,
  'nTrener' | 'nPocetHod' | 'nMaxPocetHod' | 'nVisible' | 'nLock'
> & {
  schedule: DateRange;
};

export const ReservationForm: React.FC<{
  data?: ReservationFragment;
  onSuccess?: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useGqlMutation(CreateReservationDocument, { onSuccess });
  const { mutateAsync: doUpdate } = useGqlMutation(UpdateReservationDocument, { onSuccess });
  const { data: trainers } = useGqlQuery(TrainerListDocument, {});

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
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({ input: patch });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="nTrener"
        label="Trenér"
        required
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
      <DateRangeInput control={control} name="schedule" label="Publikováno od/do" />
      <CheckboxElement control={control} name="nVisible" value="1" label="Viditelný" />
      <CheckboxElement control={control} name="nLock" value="1" label="Uzamčený" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
