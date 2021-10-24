<?php
class User
{
    public function __serialize(): array
    {
        return $this->toArray();
    }
    public function __unserialize(array $x): User
    {
        return self::fromArray($x);
    }

    public static function fromArray(array $x): User
    {
        $user = new self();
        $user->setId($x['u_id']);
        $user->setLogin($x['u_login']);
        $user->setPassword($x['u_pass']);
        $user->setName($x['u_jmeno']);
        $user->setSurname($x['u_prijmeni']);
        $user->setGender($x['u_pohlavi']);
        $user->setEmail($x['u_email']);
        $user->setPhone($x['u_telefon']);
        $user->setBirthDate($x['u_narozeni']);
        $user->setBirthNumber($x['u_rodne_cislo']);
        $user->setNotes($x['u_poznamky']);
        $user->setUpdatedAt($x['u_timestamp']);
        $user->setPermissionGroup($x['u_group']);
        $user->setTrainingGroup($x['u_skupina']);
        $user->setDancer($x['u_dancer']);
        $user->setBanned($x['u_ban']);
        $user->setLocked($x['u_lock']);
        $user->setConfirmed($x['u_confirmed']);
        $user->setSystem($x['u_system']);
        $user->setStreet($x['u_street']);
        $user->setConscriptionNumber($x['u_conscription_number']);
        $user->setOrientationNumber($x['u_orientation_number']);
        $user->setDistrict($x['u_district']);
        $user->setCity($x['u_city']);
        $user->setPostalCode($x['u_postal_code']);
        $user->setNationality($x['u_nationality']);
        $user->setMemberSince($x['u_member_since']);
        $user->setMemberUntil($x['u_member_until']);
        $user->setCreatedAt($x['u_created_at']);
        $user->setTeacher($x['u_teacher']);
        $user->setGdprSignedAt($x['u_gdpr_signed_at']);
        return $user;
    }

    public function toArray(): array
    {
        return [
            'u_id' => $this->getId(),
            'u_login' => $this->getLogin(),
            'u_pass' => $this->getPassword(),
            'u_jmeno' => $this->getName(),
            'u_prijmeni' => $this->getSurname(),
            'u_pohlavi' => $this->getGender(),
            'u_email' => $this->getEmail(),
            'u_telefon' => $this->getPhone(),
            'u_narozeni' => $this->getBirthDate(),
            'u_rodne_cislo' => $this->getBirthNumber(),
            'u_poznamky' => $this->getNotes(),
            'u_timestamp' => $this->getCreatedAt(),
            'u_group' => $this->getPermissionGroup(),
            'u_skupina' => $this->getTrainingGroup(),
            'u_dancer' => $this->getDancer(),
            'u_ban' => $this->getBanned(),
            'u_lock' => $this->getLocked(),
            'u_confirmed' => $this->getConfirmed(),
            'u_system' => $this->getSystem(),
            'u_street' => $this->getStreet(),
            'u_conscription_number' => $this->getConscriptionNumber(),
            'u_orientation_number' => $this->getOrientationNumber(),
            'u_district' => $this->getDistrict(),
            'u_city' => $this->getCity(),
            'u_postal_code' => $this->getPostalCode(),
            'u_nationality' => $this->getNationality(),
            'u_member_since' => $this->getMemberSince(),
            'u_member_until' => $this->getMemberUntil(),
            'u_created_at' => $this->getCreatedAt(),
            'u_teacher' => $this->getTeacher(),
            'u_gdpr_signed_at' => $this->getGdprSignedAt(),
        ];
    }

    public function isValid(): bool
    {
        $email = "/^[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$/i";
        $phone = "/^((\+|00)\d{3})?( ?\d{3}){3}$/";
        $birthDate = "/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/";
        return preg_match($email, $this->getEmail())
            && preg_match($phone, $this->getPhone())
            && preg_match($birthDate, $this->getBirthDate())
            && is_numeric($this->getNationality())
            && $this->getCity()
            && $this->getBirthNumber()
            && is_numeric(str_replace(' ', '', $this->getPostalCode()));
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
    protected $password;

    public function getPassword(): string
    {
        return $this->password;
    }

    public function setPassword(string $password): void
    {
        $this->password = $password;
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

    public function getFullName(): string
    {
        return $this->getSurname() . ', ' . $this->getName();
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

    public function getBirthYear(): string
    {
        return explode('-', $this->getBirthDate())[0];
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
     * @var string
     */
    protected $notes;

    public function getNotes(): string
    {
        return $this->notes;
    }

    public function setNotes(string $notes): void
    {
        $this->notes = $notes;
    }

    /**
     * @var string
     */
    protected $updatedAt;

    public function getUpdatedAt(): string
    {
        return $this->updatedAt;
    }

    public function setUpdatedAt(string $updatedAt): void
    {
        $this->updatedAt = $updatedAt;
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
     * @var bool
     */
    protected $dancer;

    public function getDancer(): bool
    {
        return $this->dancer;
    }

    public function setDancer(bool $dancer): void
    {
        $this->dancer = $dancer;
    }

    /**
     * @var bool
     */
    protected $banned;

    public function getBanned(): bool
    {
        return $this->banned;
    }

    public function setBanned(bool $banned): void
    {
        $this->banned = $banned;
    }

    /**
     * @var bool
     */
    protected $locked;

    public function getLocked(): bool
    {
        return $this->locked;
    }

    public function setLocked(bool $locked): void
    {
        $this->locked = $locked;
    }

    /**
     * @var bool
     */
    protected $confirmed;

    public function getConfirmed(): bool
    {
        return $this->confirmed;
    }

    public function setConfirmed(bool $confirmed): void
    {
        $this->confirmed = $confirmed;
    }

    /**
     * @var bool
     */
    protected $system;

    public function getSystem(): bool
    {
        return $this->system;
    }

    public function setSystem(bool $system): void
    {
        $this->system = $system;
    }

    /**
     * @var string
     */
    protected $street;

    public function getStreet(): string
    {
        return $this->street;
    }

    public function setStreet(string $street): void
    {
        $this->street = $street;
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

    /**
     * @var string|null
     */
    protected $memberSince;

    public function getMemberSince(): ?string
    {
        return $this->memberSince;
    }

    public function setMemberSince(?string $memberSince): void
    {
        $this->memberSince = $memberSince;
    }

    /**
     * @var string|null
     */
    protected $memberUntil;

    public function getMemberUntil(): ?string
    {
        return $this->memberUntil;
    }

    public function setMemberUntil(?string $memberUntil): void
    {
        $this->memberUntil = $memberUntil;
    }

    /**
     * @var string
     */
    protected $createdAt;

    public function getCreatedAt(): string
    {
        return $this->createdAt;
    }

    public function setCreatedAt(string $createdAt): void
    {
        $this->createdAt = $createdAt;
    }

    /**
     * @var bool
     */
    protected $teacher;

    public function getTeacher(): bool
    {
        return $this->teacher;
    }

    public function setTeacher(bool $teacher): void
    {
        $this->teacher = $teacher;
    }

    /**
     * @var string|null
     */
    protected $gdprSignedAt;

    public function getGdprSignedAt(): ?string
    {
        return $this->gdprSignedAt;
    }

    public function setGdprSignedAt(?string $gdprSignedAt): void
    {
        $this->gdprSignedAt = $gdprSignedAt;
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
