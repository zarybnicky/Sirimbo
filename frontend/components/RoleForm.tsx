import { RoleFragment, PermissionInput, useCreateRoleMutation, useUpdateRoleMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { useAsyncCallback } from 'react-async-hook'
import { Slider, Typography, Grid, Button } from "@mui/material";
import { allowedPermissions, defaultPermissions, PermissionKey, permissionLabels, PermissionLevel } from "lib/data/use-permissions";
import { keysOf } from "lib/keys-of";
import { Controller, TextFieldElement } from "react-hook-form-mui";

const marks = [
  { value: 0, realValue: PermissionLevel.P_NONE, label: 'žádná' },
  { value: 1, realValue: PermissionLevel.P_VIEW, label: 'zobrazení' },
  { value: 2, realValue: PermissionLevel.P_MEMBER, label: 'člen' },
  { value: 3, realValue: PermissionLevel.P_OWNED, label: 'správa svých' },
  { value: 4, realValue: PermissionLevel.P_ADMIN, label: 'správa všech' },
];

type FormProps = PermissionInput;

export const RoleForm: React.FC<{
  data?: RoleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateRoleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateRoleMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: data,
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.peId, patch: values });
    } else {
      await doCreate({
        input: { ...values, peAnkety: 1, peInzerce: 1, peKonzole: 1 },
      });
    }
  });

  const realPermissionKeys = keysOf(PermissionKey).filter(key => (!~~key && key.toString() !== "0"));
  const realPermissionLevels = keysOf(PermissionLevel).filter(key => !(!~~key && key.toString() !== "0"));

  return (
    <Grid container spacing={3} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <Grid item xs={12}>
        <TextFieldElement control={control} fullWidth name="peName" label="Jméno" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} fullWidth name="peDescription" label="Popis" multiline rows={3} required />
      </Grid>
      {realPermissionKeys.map(key => <React.Fragment key={key}>
        <Grid item xs={12} md={3}>
          <Typography gutterBottom>{permissionLabels[key]}</Typography>
        </Grid>
        <Grid item xs={12} md={9}>
          <Controller
            name={key}
            control={control}
            defaultValue={defaultPermissions[key]}
            render={({ field: { onChange, value, ...props } }) => (
              <Slider
                {...props}
                value={[1, 2, 4, 8, 16].indexOf(value as number)}
                onChange={(_, value) => onChange(parseInt(realPermissionLevels[value as number]!))}
                min={0} max={4} step={null}
                marks={marks.filter(x => allowedPermissions[key]?.includes(x.realValue))}
                valueLabelDisplay="off"
              />
            )}
          />
        </Grid>
      </React.Fragment>)}
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid>
  );
}
