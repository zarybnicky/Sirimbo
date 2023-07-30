<?php
class User
{
    public static function fromArray(array $x): User
    {
        $user = new self();
        $user->setId($x['u_id']);
        $user->setLogin($x['u_login']);
        $user->setName($x['u_jmeno']);
        $user->setSurname($x['u_prijmeni']);
        $user->setGender($x['u_pohlavi']);
        $user->setEmail($x['u_email']);
        $user->setPhone($x['u_telefon']);
        $user->setBirthDate($x['u_narozeni']);
        $user->setBirthNumber($x['u_rodne_cislo']);
        $user->setPermissionGroup($x['u_group']);
        $user->setTrainingGroup($x['u_skupina']);
        $user->setConscriptionNumber($x['u_conscription_number']);
        $user->setOrientationNumber($x['u_orientation_number']);
        $user->setDistrict($x['u_district']);
        $user->setCity($x['u_city']);
        $user->setPostalCode($x['u_postal_code']);
        $user->setNationality($x['u_nationality']);
        return $user;
    }

    /**
     * @var int
     */
    protected $id;

    public function getId(): int
    {
        return $this->id;
    }

    public function setId(int $id): void
    {
        $this->id = $id;
    }

    /**
     * @var string
     */
    protected $login;

    public function getLogin(): string
    {
        return $this->login;
    }

    public function setLogin(string $login): void
    {
        $this->login = $login;
    }

    /**
     * @var string
     */
    protected $name;

    public function getName(): string
    {
        return $this->name;
    }

    public function setName(string $name): void
    {
        $this->name = $name;
    }

    /**
     * @var string
     */
    protected $surname;

    public function getSurname(): string
    {
        return $this->surname;
    }

    public function setSurname(string $surname): void
    {
        $this->surname = $surname;
    }

    /**
     * @var string
     */
    protected $gender;

    public function getGender(): string
    {
        return $this->gender;
    }

    public function setGender(string $gender): void
    {
        $this->gender = $gender;
    }

    /**
     * @var string
     */
    protected $email;

    public function getEmail(): string
    {
        return $this->email;
    }

    public function setEmail(string $email): void
    {
        $this->email = $email;
    }

    /**
     * @var string
     */
    protected $phone;

    public function getPhone(): string
    {
        return $this->phone;
    }

    public function setPhone(string $phone): void
    {
        $this->phone = $phone;
    }

    /**
     * @var string
     */
    protected $birthDate;

    public function getBirthDate(): string
    {
        return $this->birthDate;
    }

    public function setBirthDate(string $birthDate): void
    {
        $this->birthDate = $birthDate;
    }

    /**
     * @var string|null
     */
    protected $birthNumber;

    public function getBirthNumber(): ?string
    {
        return $this->birthNumber;
    }

    public function setBirthNumber(?string $birthNumber): void
    {
        $this->birthNumber = $birthNumber;
    }

    /**
     * @var int
     */
    protected $permissionGroup = 0;

    public function getPermissionGroup(): int
    {
        return $this->permissionGroup;
    }

    public function setPermissionGroup(int $permissionGroup): void
    {
        $this->permissionGroup = $permissionGroup;
    }

    /**
     * @var int
     */
    protected $trainingGroup;

    public function getTrainingGroup(): int
    {
        return $this->trainingGroup;
    }

    public function setTrainingGroup(int $trainingGroup): void
    {
        $this->trainingGroup = $trainingGroup;
    }

    /**
     * @var string
     */
    protected $conscriptionNumber;

    public function getConscriptionNumber(): string
    {
        return $this->conscriptionNumber;
    }

    public function setConscriptionNumber(string $conscriptionNumber): void
    {
        $this->conscriptionNumber = $conscriptionNumber;
    }

    /**
     * @var string
     */
    protected $orientationNumber;

    public function getOrientationNumber(): string
    {
        return $this->orientationNumber;
    }

    public function setOrientationNumber(string $orientationNumber): void
    {
        $this->orientationNumber = $orientationNumber;
    }

    /**
     * @var string
     */
    protected $district;

    public function getDistrict(): string
    {
        return $this->district;
    }

    public function setDistrict(string $district): void
    {
        $this->district = $district;
    }

    /**
     * @var string
     */
    protected $city;

    public function getCity(): string
    {
        return $this->city;
    }

    public function setCity(string $city): void
    {
        $this->city = $city;
    }

    /**
     * @var string
     */
    protected $postalCode;

    public function getPostalCode(): string
    {
        return $this->postalCode;
    }

    public function setPostalCode(string $postalCode): void
    {
        $this->postalCode = $postalCode;
    }

    /**
     * @var string
     */
    protected $nationality;

    public function getNationality(): string
    {
        return $this->nationality;
    }

    public function setNationality(string $nationality): void
    {
        $this->nationality = $nationality;
    }

    public static function crypt(string $passwd): string
    {
        $fix = md5('######TK.-.OLYMP######');
        return sha1($fix . $passwd . $fix);
    }

    public function getVarSymbol(): string
    {
        return str_pad((string) $this->id, 6, '0', STR_PAD_LEFT);
    }
}
