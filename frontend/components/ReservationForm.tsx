import { ReservationFragment, useCreateReservationMutation, useUpdateReservationMutation } from 'lib/graphql/Reservation';
import React from 'react';
import { useForm } from 'react-hook-form';
import { SelectElement } from 'components/SelectElement';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { NabidkaInput } from 'lib/graphql';
import { useTrainerListQuery } from 'lib/graphql/User';
import { DateRange, DateRangeInput } from './DateRange';

type FormProps = Pick<NabidkaInput, 'nTrener' | 'nPocetHod' | 'nMaxPocetHod' | 'nVisible' | 'nLock'> & {
  schedule: DateRange;
};;

export const ReservationForm: React.FC<{
  data?: ReservationFragment;
  onSuccess?: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateReservationMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateReservationMutation({ onSuccess });

  const { data: trainers } = useTrainerListQuery();

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      nTrener: data?.nTrener,
      nPocetHod: data?.nPocetHod,
      nMaxPocetHod: data?.nMaxPocetHod,
      nVisible: data?.nVisible,
      nLock: data?.nLock,
      schedule: [
        data?.nOd ? new Date(data?.nOd) : undefined,
        data?.nDo ? new Date(data?.nDo) : undefined,
      ],
    });
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      nTrener: values.nTrener,
      nPocetHod: values.nPocetHod,
      nMaxPocetHod: values.nMaxPocetHod,
      nVisible: values.nVisible,
      nLock: values.nLock,
      nOd: values.schedule[0]?.toISOString(),
      nDo: values.schedule[1]?.toDateString(),
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
      <SelectElement
        control={control} name="nTrener" label="Trenér" required
        options={(trainers?.trainers?.nodes || []).map(x => ({ id: x.id, label: `${x.uJmeno} ${x.uPrijmeni}` }))}
      />
      <TextFieldElement control={control} name="nPocetHod" label="Počet hodin" required />
      <TextFieldElement control={control} name="nMaxPocetHod" label="Max.počet hodin" required />
      <DateRangeInput control={control} name="schedule" label="Publikováno od/do" />
      <CheckboxElement control={control} name="nVisible" value="1" label="Viditelný" />
      <CheckboxElement control={control} name="nLock" value="1" label="Uzamčený" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
