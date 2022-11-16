import * as React from 'react';
import { saveAs } from 'file-saver';
import { useMemberListQuery } from 'lib/graphql';
import { PermissionKey, PermissionLevel, usePermissions } from 'lib/data/use-permissions';
import { Button } from './Button';

export function CohortExport({ id, name }: { id?: string; name?: string; }) {
  const perms = usePermissions();
  const saveData = React.useCallback(async (e?: React.MouseEvent) => {
    e?.preventDefault();

    const { Workbook } = await import('exceljs');
    const data = await useMemberListQuery.fetcher({ cohortId: id })();
    const workbook = new Workbook();
    const worksheet = workbook.addWorksheet(name || "Sheet 1");

    worksheet.columns = [
      { header: 'Jméno', key: 'firstName' },
      { header: 'Přijmení', key: 'lastName' },
      { header: 'Rodné číslo', key: 'birthNumber' },
      { header: 'Telefon', key: 'phone' },
      { header: 'E-mail', key: 'email' },
    ];

    worksheet.getRow(1).font = { bold: true };
    worksheet.columns.forEach(column => {
      column.width = (column?.header?.length || 0) + 10;
      column.alignment = { horizontal: 'center' };
    });

    data.members?.nodes.forEach(x => worksheet.addRow({
      firstName: x.uJmeno,
      lastName: x.uPrijmeni,
      birthNumber: x.uRodneCislo,
      phone: x.uTelefon,
      email: x.uEmail,
    }));

    const buf = await workbook.xlsx.writeBuffer();
    saveAs(new Blob([buf]), `${name || "Všechny skupiny"}.xlsx`);
  }, [id, name]);

  if (!perms.hasPermission(PermissionKey.peRozpis, PermissionLevel.P_OWNED)) {
    return null;
  }

  return <Button onClick={saveData}>Export {!name && 'všech'}</Button>;
}
