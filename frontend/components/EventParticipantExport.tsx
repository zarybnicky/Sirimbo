import * as React from 'react';
import { saveAs } from 'file-saver';
import { useEventParticipantsQuery } from 'lib/graphql';
import { Button } from './Button';

export function EventParticipantExport({ id }: { id: string; }) {
  const { data } = useEventParticipantsQuery({ id });

  const saveData = React.useCallback(async (e?: React.MouseEvent) => {
    e?.preventDefault();
    if (!data) {
      return;
    }
    const { Workbook } = await import('exceljs');
    const workbook = new Workbook();
    const worksheet = workbook.addWorksheet(data.akce?.aJmeno || "Sheet 1");

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

    data.akce?.akceItemsByAiIdRodic.nodes.forEach(x => worksheet.addRow({
      firstName: x.userByAiUser?.uJmeno,
      lastName: x.userByAiUser?.uPrijmeni,
      birthNumber: x.userByAiUser?.uRodneCislo,
      phone: x.userByAiUser?.uTelefon,
      email: x.userByAiUser?.uEmail,
    }));

    const buf = await workbook.xlsx.writeBuffer();
    saveAs(new Blob([buf]), `${data.akce?.aJmeno || "export-akce"}.xlsx`);
  }, [data]);

  return <Button onClick={saveData}>Export přihlášených</Button>;
}
