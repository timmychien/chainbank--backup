pragma solidity ^0.5.0;
pragma experimental ABIEncoderV2;
library SafeMath {
    /**
     * @dev Returns the addition of two unsigned integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `+` operator.
     *
     * Requirements:
     * - Addition cannot overflow.
     */
    function add(uint256 a, uint256 b) internal pure returns (uint256) {
        uint256 c = a + b;
        require(c >= a, "SafeMath: addition overflow");

        return c;
    }

    /**
     * @dev Returns the subtraction of two unsigned integers, reverting on
     * overflow (when the result is negative).
     *
     * Counterpart to Solidity's `-` operator.
     *
     * Requirements:
     * - Subtraction cannot overflow.
     */
    function sub(uint256 a, uint256 b) internal pure returns (uint256) {
        return sub(a, b, "SafeMath: subtraction overflow");
    }

    /**
     * @dev Returns the subtraction of two unsigned integers, reverting with custom message on
     * overflow (when the result is negative).
     *
     * Counterpart to Solidity's `-` operator.
     *
     * Requirements:
     * - Subtraction cannot overflow.
     *
     * _Available since v2.4.0._
     */
    function sub(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        require(b <= a, errorMessage);
        uint256 c = a - b;

        return c;
    }

    /**
     * @dev Returns the multiplication of two unsigned integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `*` operator.
     *
     * Requirements:
     * - Multiplication cannot overflow.
     */
    function mul(uint256 a, uint256 b) internal pure returns (uint256) {
        // Gas optimization: this is cheaper than requiring 'a' not being zero, but the
        // benefit is lost if 'b' is also tested.
        // See: https://github.com/OpenZeppelin/openzeppelin-contracts/pull/522
        if (a == 0) {
            return 0;
        }

        uint256 c = a * b;
        require(c / a == b, "SafeMath: multiplication overflow");

        return c;
    }

    /**
     * @dev Returns the integer division of two unsigned integers. Reverts on
     * division by zero. The result is rounded towards zero.
     *
     * Counterpart to Solidity's `/` operator. Note: this function uses a
     * `revert` opcode (which leaves remaining gas untouched) while Solidity
     * uses an invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     */
    function div(uint256 a, uint256 b) internal pure returns (uint256) {
        return div(a, b, "SafeMath: division by zero");
    }

    /**
     * @dev Returns the integer division of two unsigned integers. Reverts with custom message on
     * division by zero. The result is rounded towards zero.
     *
     * Counterpart to Solidity's `/` operator. Note: this function uses a
     * `revert` opcode (which leaves remaining gas untouched) while Solidity
     * uses an invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     *
     * _Available since v2.4.0._
     */
    function div(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        // Solidity only automatically asserts when dividing by 0
        require(b > 0, errorMessage);
        uint256 c = a / b;
        // assert(a == b * c + a % b); // There is no case in which this doesn't hold

        return c;
    }

    /**
     * @dev Returns the remainder of dividing two unsigned integers. (unsigned integer modulo),
     * Reverts when dividing by zero.
     *
     * Counterpart to Solidity's `%` operator. This function uses a `revert`
     * opcode (which leaves remaining gas untouched) while Solidity uses an
     * invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     */
    function mod(uint256 a, uint256 b) internal pure returns (uint256) {
        return mod(a, b, "SafeMath: modulo by zero");
    }

    /**
     * @dev Returns the remainder of dividing two unsigned integers. (unsigned integer modulo),
     * Reverts with custom message when dividing by zero.
     *
     * Counterpart to Solidity's `%` operator. This function uses a `revert`
     * opcode (which leaves remaining gas untouched) while Solidity uses an
     * invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     *
     * _Available since v2.4.0._
     */
    function mod(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        require(b != 0, errorMessage);
        return a % b;
    }
}
contract Ownable {
  address public owner;

  event transferOwner(address indexed existingOwner, address indexed newOwner);

  constructor() public {
    owner = msg.sender;
  }

  modifier onlyOwner() {
    require(msg.sender == owner);
    _;
  }

  function transferOwnership(address newOwner) onlyOwner public {
    if (newOwner != address(0)) {
      owner = newOwner;
      emit transferOwner(msg.sender, owner);
    }
  }
}

contract ERC20Basic {
  function totalSupply() public view returns (uint256);
  function balanceOf(address who) public view returns (uint256);
  function transfer(address to, uint256 value) public returns (bool);
  event Transfer(address indexed from, address indexed to, uint256 value);
}

contract ERC20 is ERC20Basic {
  function allowance(address owner, address spender) public view returns (uint256);
  function transferFrom(address from, address to, uint256 value) public returns (bool);
  function approve(address spender, uint256 value) public returns (bool);
  event Approval(address indexed owner, address indexed spender, uint256 value);
}

contract BasicToken is ERC20Basic {
  using SafeMath for uint256;

  mapping(address => uint256) balances;

  uint256 totalSupply_;

  /**
  * @dev total number of tokens in existence
  */
  function totalSupply() public view returns (uint256) {
    return totalSupply_;
  }

  /**
  * @dev transfer token for a specified address
  * @param _to The address to transfer to.
  * @param _value The amount to be transferred.
  */
  function transfer(address _to, uint256 _value) public returns (bool) {
    require(_to != address(0));
    require(_value <= balances[msg.sender]);

    balances[msg.sender] = balances[msg.sender].sub(_value);
    balances[_to] = balances[_to].add(_value);
    emit Transfer(msg.sender, _to, _value);
    return true;
  }

  /**
  * @dev Gets the balance of the specified address.
  * @param _owner The address to query the the balance of.
  * @return An uint256 representing the amount owned by the passed address.
  */
  function balanceOf(address _owner) public view returns (uint256) {
    return balances[_owner];
  }

}

contract StandardToken is ERC20, BasicToken {

  mapping (address => mapping (address => uint256)) internal allowed;


  /**
   * @dev Transfer tokens from one address to another
   * @param _from address The address which you want to send tokens from
   * @param _to address The address which you want to transfer to
   * @param _value uint256 the amount of tokens to be transferred
   */
  function transferFrom(address _from, address _to, uint256 _value) public returns (bool) {
    require(_to != address(0));
    require(_value <= balances[_from]);
    require(_value <= allowed[_from][msg.sender]);

    balances[_from] = balances[_from].sub(_value);
    balances[_to] = balances[_to].add(_value);
    allowed[_from][msg.sender] = allowed[_from][msg.sender].sub(_value);
    emit Transfer(_from, _to, _value);
    return true;
  }
 /**
   * @dev Approve the passed address to spend the specified amount of tokens on behalf of msg.sender.
   *
   * Beware that changing an allowance with this method brings the risk that someone may use both the old
   * and the new allowance by unfortunate transaction ordering. One possible solution to mitigate this
   * race condition is to first reduce the spender's allowance to 0 and set the desired value afterwards:
   * https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729
   * @param _spender The address which will spend the funds.
   * @param _value The amount of tokens to be spent.
   */
  function approve(address _spender, uint256 _value) public returns (bool) {
    allowed[msg.sender][_spender] = _value;
    emit Approval(msg.sender, _spender, _value);
    return true;
  }
 /**
     * @dev Sets `amount` as the allowance of `spender` over the `owner`s tokens.
     *
     * This is internal function is equivalent to `approve`, and can be used to
     * e.g. set automatic allowances for certain subsystems, etc.
     *
     * Emits an {Approval} event.
     *
     * Requirements:
     *
     * - `owner` cannot be the zero address.
     * - `spender` cannot be the zero address.
     */
    function _approve(address owner, address spender, uint256 amount) public returns(bool){
        require(owner != address(0), "ERC20: approve from the zero address");
        require(spender != address(0), "ERC20: approve to the zero address");
        allowed[owner][spender] = amount;
        emit Approval(owner, spender, amount);
        return true;
    }
  /**
   * @dev Function to check the amount of tokens that an owner allowed to a spender.
   * @param _owner address The address which owns the funds.
   * @param _spender address The address which will spend the funds.
   * @return A uint256 specifying the amount of tokens still available for the spender.
   */
  function allowance(address _owner, address _spender) public view returns (uint256) {
    return allowed[_owner][_spender];
  }
   /**
   * @dev Internal function that burns an amount of the token of a given
   * account.
   * @param account The account whose tokens will be burnt.
   * @param amount The amount that will be burnt.
   */
  function burn(address account, uint256 amount) internal {
    require(account != address(0));
    require(amount <= balances[account]);

    totalSupply_ = totalSupply_.sub(amount);
    balances[account] = balances[account].sub(amount);
    emit Transfer(account, address(0), amount);
  }

  /**
   * @dev Internal function that burns an amount of the token of a given
   * account, deducting from the sender's allowance for said account. Uses the
   * internal burn function.
   * @param account The account whose tokens will be burnt.
   * @param amount The amount that will be burnt.
   */
  function burnFrom(address account, uint256 amount)public {
    require(amount <= allowed[account][msg.sender]);

    // Should https://github.com/OpenZeppelin/zeppelin-solidity/issues/707 be accepted,
    // this function needs to emit an event with the updated approval.
    allowed[account][msg.sender] = allowed[account][msg.sender].sub(
      amount);
    burn(account, amount);
  }
  /**
   * @dev Increase the amount of tokens that an owner allowed to a spender.
   *
   * approve should be called when allowed[_spender] == 0. To increment
   * allowed value is better to use this function to avoid 2 calls (and wait until
   * the first transaction is mined)
   * From MonolithDAO Token.sol
   * @param _spender The address which will spend the funds.
   * @param _addedValue The amount of tokens to increase the allowance by.
   */
  function increaseApproval(address _spender, uint _addedValue) public returns (bool) {
    allowed[msg.sender][_spender] = allowed[msg.sender][_spender].add(_addedValue);
    emit Approval(msg.sender, _spender, allowed[msg.sender][_spender]);
    return true;
  }

  /**
   * @dev Decrease the amount of tokens that an owner allowed to a spender.
   *
   * approve should be called when allowed[_spender] == 0. To decrement
   * allowed value is better to use this function to avoid 2 calls (and wait until
   * the first transaction is mined)
   * From MonolithDAO Token.sol
   * @param _spender The address which will spend the funds.
   * @param _subtractedValue The amount of tokens to decrease the allowance by.
   */
  function decreaseApproval(address _spender, uint _subtractedValue) public returns (bool) {
    uint oldValue = allowed[msg.sender][_spender];
    if (_subtractedValue > oldValue) {
      allowed[msg.sender][_spender] = 0;
    } else {
      allowed[msg.sender][_spender] = oldValue.sub(_subtractedValue);
    }
    emit Approval(msg.sender, _spender, allowed[msg.sender][_spender]);
    return true;
  }

}

contract ERC865 {

    function transferPreSigned(
        bytes  memory _signature,
        address _to,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);

    function approvePreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);

    function increaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _addedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);

    function decreaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _subtractedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);
}

contract ERC865Token is ERC865, StandardToken, Ownable {

    /* Nonces of transfers performed */
    mapping(bytes => bool) signatures;
    /* mapping of nonces of each user */
    mapping (address => uint256) nonces;
    
    event TransferPreSigned_Transfer_Fee(address indexed from, address indexed to, uint256 value);
    event TransferPreSigned(address indexed from, address indexed to, address indexed delegate, uint256 amount, uint256 fee);
    event ApprovalPreSigned(address indexed from, address indexed to, address indexed delegate, uint256 amount, uint256 fee);

    bytes4 internal constant transferSig = 0x48664c16;
    bytes4 internal constant approvalSig = 0xf7ac9c2e;
    bytes4 internal constant increaseApprovalSig = 0xa45f71ff;
    bytes4 internal constant decreaseApprovalSig = 0x59388d78;
    
    function approve_and_transfer(address owner,address spender,uint256 amount)public returns(bool){
        _approve(owner,spender,amount);
        transferFrom(owner,spender,amount);
    }
    function transfer_with_approve(address _from ,address to,uint256 amount)public returns(bool){
        approve(_from,amount);
        transferFrom(_from,to,amount);
    }
    //return nonce using function
    function getNonce(address _owner) public view returns (uint256 nonce){
      return nonces[_owner];
    }

    /**
     * @notice Submit a presigned transfer
     * @param _signature bytes The signature, issued by the owner.
     * @param _to address The address which you want to transfer to.
     * @param _value uint256 The amount of tokens to be transferred.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
     //this 'to' is sending token destination , not deligate address
    function transferPreSigned(
        bytes memory _signature,
        address _to,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_to != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), transferSig, _to, _value, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        require(_value.add(_fee) <= balances[from]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        balances[from] = balances[from].sub(_value).sub(_fee);
        balances[_to] = balances[_to].add(_value);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Transfer(from, _to, _value);
        emit TransferPreSigned_Transfer_Fee(from, msg.sender, _fee);
        emit TransferPreSigned(from, _to, msg.sender, _value, _fee);
        return true;
    }

    /**
     * @notice Submit a presigned approval
     * @param _signature bytes The signature, issued by the owner.
     * @param _spender address The address which will spend the funds.
     * @param _value uint256 The amount of tokens to allow.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
    function approvePreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_spender != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), approvalSig, _spender, _value, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        require(_value.add(_fee) <= balances[from]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        allowed[from][_spender] =_value;
        balances[from] = balances[from].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Approval(from, _spender, _value);
        emit Transfer(from, msg.sender, _fee);
        emit ApprovalPreSigned(from, _spender, msg.sender, _value, _fee);
        return true;
    }

    /**
     * @notice Increase the amount of tokens that an owner allowed to a spender.
     * @param _signature bytes The signature, issued by the owner.
     * @param _spender address The address which will spend the funds.
     * @param _addedValue uint256 The amount of tokens to increase the allowance by.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
    function increaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _addedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_spender != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), increaseApprovalSig, _spender, _addedValue, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        require(allowed[from][_spender].add(_addedValue).add(_fee) <= balances[from]);
        //require(_addedValue <= allowed[from][_spender]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        allowed[from][_spender] = allowed[from][_spender].add(_addedValue);
        balances[from] = balances[from].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Approval(from, _spender, allowed[from][_spender]);
        emit Transfer(from, msg.sender, _fee);
        emit ApprovalPreSigned(from, _spender, msg.sender, allowed[from][_spender], _fee);
        return true;
    }

    /**
     * @notice Decrease the amount of tokens that an owner allowed to a spender.
     * @param _signature bytes The signature, issued by the owner
     * @param _spender address The address which will spend the funds.
     * @param _subtractedValue uint256 The amount of tokens to decrease the allowance by.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
    function decreaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _subtractedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_spender != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), decreaseApprovalSig, _spender, _subtractedValue, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        //require(_subtractedValue <= balances[from]);
        //require(_subtractedValue <= allowed[from][_spender]);
        //require(_subtractedValue <= allowed[from][_spender]);
        require(_fee <= balances[from]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        uint oldValue = allowed[from][_spender];
        if (_subtractedValue > oldValue) {
            allowed[from][_spender] = 0;
        } else {
            allowed[from][_spender] = oldValue.sub(_subtractedValue);
        }
        balances[from] = balances[from].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Approval(from, _spender, _subtractedValue);
        emit Transfer(from, msg.sender, _fee);
        emit ApprovalPreSigned(from, _spender, msg.sender, allowed[from][_spender], _fee);
        return true;
    }

    /**
     * @notice Transfer tokens from one address to another
     * @param _signature bytes The signature, issued by the spender.
     * @param _from address The address which you want to send tokens from.
     * @param _to address The address which you want to transfer to.
     * @param _value uint256 The amount of tokens to be transferred.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the spender.
     * @param _nonce uint256 Presigned transaction number.
     */
    /*function transferFromPreSigned(
        bytes _signature,
        address _from,
        address _to,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_to != address(0));
        require(signatures[_signature] == false);
        signatures[_signature] = true;

        bytes32 hashedTx = transferFromPreSignedHashing(address(this), _from, _to, _value, _fee, _nonce);

        address spender = recover(hashedTx, _signature);
        require(spender != address(0));
        require(_value.add(_fee) <= balances[_from])​;

        balances[_from] = balances[_from].sub(_value);
        balances[_to] = balances[_to].add(_value);
        allowed[_from][spender] = allowed[_from][spender].sub(_value);

        balances[spender] = balances[spender].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Transfer(_from, _to, _value);
        emit Transfer(spender, msg.sender, _fee);
        return true;
    }*/

     /**
      * @notice Hash (keccak256) of the payload used by recoverPreSignedHash
      * @param _token address The address of the token
      * @param _spender address The address which will spend the funds.
      * @param _value uint256 The amount of tokens.
      * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
      * @param _nonce uint256 Presigned transaction number.
      */
      //this 'spender' is sending token destination , not deligate address
    function recoverPreSignedHash(
        address _token,
        bytes4 _functionSig,
        address _spender,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
        )
      public pure returns (bytes32)
      {
        //return keccak256(_token, _functionSig, _spender, _value, _fee, _nonce);
        return keccak256(abi.encodePacked(_token, _functionSig, _spender, _value, _fee, _nonce));
    }

    /**
     * @notice Recover signer address from a message by using his signature
     * @param hash bytes32 message, the hash is the signed message. What is recovered is the signer address.
     * @param sig bytes signature, the signature is generated using web3.eth.sign()
     */
    function recover(bytes32 hash, bytes memory sig) public pure returns (address) {
      bytes32 r;
      bytes32 s;
      uint8 v;

      //Check the signature length
      if (sig.length != 65) {
        return (address(0));
      }

      // Divide the signature in r, s and v variables
      assembly {
        r := mload(add(sig, 32))
        s := mload(add(sig, 64))
        v := byte(0, mload(add(sig, 96)))
      }

      // Version of signature should be 27 or 28, but 0 and 1 are also possible versions
      if (v < 27) {
        v += 27;
      }

      // If the version is correct return the signer address
      if (v != 27 && v != 28) {
        return (address(0));
      } else {
        return ecrecover(hash, v, r, s);
      }
    }

}




// 資金需求人智能合約
// 標的
library DateTime {
        /*
         *  Date and Time utilities for ethereum contracts
         *
         */
        struct _DateTime {
                uint16 year;
                uint8 month;
                uint8 day;
                uint8 hour;
                uint8 minute;
                uint8 second;
                uint8 weekday;
        }

        uint constant DAY_IN_SECONDS = 86400;
        uint constant YEAR_IN_SECONDS = 31536000;
        uint constant LEAP_YEAR_IN_SECONDS = 31622400;

        uint constant HOUR_IN_SECONDS = 3600;
        uint constant MINUTE_IN_SECONDS = 60;

        uint16 constant ORIGIN_YEAR = 1970;

        function isLeapYear(uint16 year) internal pure returns (bool) {
                if (year % 4 != 0) {
                        return false;
                }
                if (year % 100 != 0) {
                        return true;
                }
                if (year % 400 != 0) {
                        return false;
                }
                return true;
        }

        function leapYearsBefore(uint year) internal pure returns (uint) {
                year -= 1;
                return year / 4 - year / 100 + year / 400;
        }

        function getDaysInMonth(uint8 month, uint16 year) internal pure returns (uint8) {
                if (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) {
                        return 31;
                }
                else if (month == 4 || month == 6 || month == 9 || month == 11) {
                        return 30;
                }
                else if (isLeapYear(year)) {
                        return 29;
                }
                else {
                        return 28;
                }
        }

        function parseTimestamp(uint timestamp) internal pure returns (_DateTime memory dt) {
                uint secondsAccountedFor = 0;
                uint buf;
                uint8 i;

                // Year
                dt.year = getYear(timestamp);
                buf = leapYearsBefore(dt.year) - leapYearsBefore(ORIGIN_YEAR);

                secondsAccountedFor += LEAP_YEAR_IN_SECONDS * buf;
                secondsAccountedFor += YEAR_IN_SECONDS * (dt.year - ORIGIN_YEAR - buf);

                // Month
                uint secondsInMonth;
                for (i = 1; i <= 12; i++) {
                        secondsInMonth = DAY_IN_SECONDS * getDaysInMonth(i, dt.year);
                        if (secondsInMonth + secondsAccountedFor > timestamp) {
                                dt.month = i;
                                break;
                        }
                        secondsAccountedFor += secondsInMonth;
                }

                // Day
                for (i = 1; i <= getDaysInMonth(dt.month, dt.year); i++) {
                        if (DAY_IN_SECONDS + secondsAccountedFor > timestamp) {
                                dt.day = i;
                                break;
                        }
                        secondsAccountedFor += DAY_IN_SECONDS;
                }

                // Hour
                dt.hour = getHour(timestamp);

                // Minute
                dt.minute = getMinute(timestamp);

                // Second
                dt.second = getSecond(timestamp);

                // Day of week.
                dt.weekday = getWeekday(timestamp);
        }

        function getYear(uint timestamp) internal pure returns (uint16) {
                uint secondsAccountedFor = 0;
                uint16 year;
                uint numLeapYears;

                // Year
                year = uint16(ORIGIN_YEAR + timestamp / YEAR_IN_SECONDS);
                numLeapYears = leapYearsBefore(year) - leapYearsBefore(ORIGIN_YEAR);

                secondsAccountedFor += LEAP_YEAR_IN_SECONDS * numLeapYears;
                secondsAccountedFor += YEAR_IN_SECONDS * (year - ORIGIN_YEAR - numLeapYears);

                while (secondsAccountedFor > timestamp) {
                        if (isLeapYear(uint16(year - 1))) {
                                secondsAccountedFor -= LEAP_YEAR_IN_SECONDS;
                        }
                        else {
                                secondsAccountedFor -= YEAR_IN_SECONDS;
                        }
                        year -= 1;
                }
                return year;
        }

        function getMonth(uint timestamp) internal pure returns (uint8) {
                return parseTimestamp(timestamp).month;
        }

        function getDay(uint timestamp) internal pure returns (uint8) {
                return parseTimestamp(timestamp).day;
        }

        function getHour(uint timestamp) internal pure returns (uint8) {
                return uint8((timestamp / 60 / 60) % 24);
        }

        function getMinute(uint timestamp) internal pure returns (uint8) {
                return uint8((timestamp / 60) % 60);
        }

        function getSecond(uint timestamp) internal pure returns (uint8) {
                return uint8(timestamp % 60);
        }

        function getWeekday(uint timestamp) internal pure returns (uint8) {
                return uint8((timestamp / DAY_IN_SECONDS + 4) % 7);
        }

        function toTimestamp(uint16 year, uint8 month, uint8 day) internal pure returns (uint timestamp) {
                return toTimestamp(year, month, day, 0, 0, 0);
        }

        function toTimestamp(uint16 year, uint8 month, uint8 day, uint8 hour) internal pure returns (uint timestamp) {
                return toTimestamp(year, month, day, hour, 0, 0);
        }

        function toTimestamp(uint16 year, uint8 month, uint8 day, uint8 hour, uint8 minute) internal pure returns (uint timestamp) {
                return toTimestamp(year, month, day, hour, minute, 0);
        }

        function toTimestamp(uint16 year, uint8 month, uint8 day, uint8 hour, uint8 minute, uint8 second) internal pure returns (uint timestamp) {
                uint16 i;

                // Year
                for (i = ORIGIN_YEAR; i < year; i++) {
                        if (isLeapYear(i)) {
                                timestamp += LEAP_YEAR_IN_SECONDS;
                        }
                        else {
                                timestamp += YEAR_IN_SECONDS;
                        }
                }

                // Month
                uint8[12] memory monthDayCounts;
                monthDayCounts[0] = 31;
                if (isLeapYear(year)) {
                        monthDayCounts[1] = 29;
                }
                else {
                        monthDayCounts[1] = 28;
                }
                monthDayCounts[2] = 31;
                monthDayCounts[3] = 30;
                monthDayCounts[4] = 31;
                monthDayCounts[5] = 30;
                monthDayCounts[6] = 31;
                monthDayCounts[7] = 31;
                monthDayCounts[8] = 30;
                monthDayCounts[9] = 31;
                monthDayCounts[10] = 30;
                monthDayCounts[11] = 31;

                for (i = 1; i < month; i++) {
                        timestamp += DAY_IN_SECONDS * monthDayCounts[i - 1];
                }

                // Day
                timestamp += DAY_IN_SECONDS * (day - 1);

                // Hour
                timestamp += HOUR_IN_SECONDS * (hour);

                // Minute
                timestamp += MINUTE_IN_SECONDS * (minute);

                // Second
                timestamp += second;

                return timestamp;
        }
}

contract FundingDemand is ERC865Token{
	using SafeMath for uint;
    using DateTime for DateTime._DateTime;
    
/*  版本說明
    版本: 0.1.0
    主要內容:
    
*/

//  資金需求人

// 資產類別 struct collateral
	//address borrower;
	//平台帳戶
	address platform=0x63A40281087c53479382283dE03d64A83f5C7df0;
	//平台幣
	address platform_token=0xf8Aa8c066fF48338Df1a775491BfC0dc093c7329;
	//address collateralHolder; //擔保資產持有人
	struct Collateral {
		bool class; // 0=動產 1=不動產 
		bool market; //0=無市價, 1=有市價
		bool lock; //0=不移轉, 1=移轉, 移轉需要多簽機制
		bool validate; //0=未驗證, 1=已驗證
		bool isdefault; //0=未違約, 1=已違約
		uint valueSrcId; //價值來源代碼 (由中心化資料庫定義並儲存)
		uint value; //資產價值
		uint cashYear; //資產現金流量年期, 0=lump sum 無年期
		uint[] cashIn; //現金流入[月]
		uint[] cashOut; //現金流出[月]
		uint subDebtNum; //次貸次數
		uint[] subDebtAmount; //每次次貸金額[次數]
	}
//	collateral[collateralHolder][collateral id]
	mapping(address => mapping(uint => Collateral)) public collateral;

//質押設定

// collateralHolder 的 collaterId 號的資產 的第 registerNum 次貸款 
	mapping (address => mapping(uint => uint)) public registerNum;
//  上述資產本次增加設定金額
	mapping(address => mapping(uint => uint[])) public register;
//借貸方token擁有數量
    mapping(address=>uint256)balances;
/* 
	提供擔保資產 function createCollateral
	驗證資產真實性 function validateCollateral
	確認資產價值 (市價, 第三方提供) function getValue
	確認需求模式 (不足額: 一般借貸, 超額: 保證金或融資融券)
	   需求金額, 還款年期, 還款方式(本利攤, 先還利息及年期) function getDemand
	資產擔保設定並鎖入 function regCollateral
	計算需求現值 function calcPV
    
*/
    event deposit(address indexed borrower, uint256 value);
    event locked(address indexed borrower,uint256 creditAmount);
    event unlocked(address indexed borrower,uint256 creditAMount);
    event margin_credit(uint256 credit_id,address indexed borrower,uint256 margin);
    //event transfer(address indexed _from,address indexed _to,uint256 value);
//Modifier
    modifier BorrowerExist(address user){
        require(user!=address(0));
        _;
    }
    modifier HolderExist(address holder){
        require(holder!=address(0));
        _;
    }
    constructor()public{
        
    }
    //存平台幣
        function pay_margin(address borrower,uint amount)public BorrowerExist(borrower)  returns(bool){
            bytes memory deposit_token = abi.encodeWithSignature("transfer_with_approve(address,address,uint256)",borrower,platform,amount);
	        platform_token.call(deposit_token);
	      
            //platform_token.delegatecall(abi.encodePacked(bytes4(keccak256("transferFrom(address,address,uint256)")), borrower,platform,amount));
	        //emit deposit(borrower,amount);
            return true;
        }
        /*
         function tokenburn(uint256 value)public {
            require(value<=balances[borrower]);
            ERC865Token token=ERC865Token(0x72682d0d54c7ED7cdDdAa66E6DD7171f2B9c626C);
            //_approve(platform,borrower,value);
            token.transferFrom(platform,address(0),value);
            totalSupply_=totalSupply_.sub(value);
            emit Transfer(borrower,address(0),value);
        }
        */
        
        function createCollateral(address collateralHolder,uint collateralId)internal HolderExist(collateralHolder)returns(uint){
            
        }
        function validateCollateral(uint collaterId)internal pure returns(bool){
            
        }
        function getValue(uint collateralId)internal pure returns(uint){
            
        }
        /*
        function getDemand( )internal pure returns(){
            
        }
        */
        function regCollateral(uint collateralId)internal pure returns(bool){
            
        }
        //生成信用憑證
        function createCredit(address borrower)internal pure returns(uint){
            //uint creditAmount=collateral[collateralId][collateralValue];
            //return creditAmount
        }
        function lockCredit(address borrower,uint256 creditAmount)internal 
        BorrowerExist(borrower) 
    
        pure
        returns(uint){

        }
        function tokensUnlockable(address borrower)internal view returns (uint amount){
            
        }
        /*
        function unlockCredit(address borrower,uint256 creditAmount)internal 
        BorrowerExist(borrower)
        pure
        returns(uint unlocked){
                
        }
        */
}
contract Credit is ERC865Token{
    using SafeMath for uint256;
    //ERC865Token credit;
    //ERC865Token platform_token;
    //ERC865Token platform_token=ERC865Token(0x9269febE57B574dF6F8e9E53B2EEE31d94B87559);
    string constant public name="Credit" ;
    string constant public symbol="credit";
    uint8 public constant decimals = 0;
    uint256 public constant Initial_Supply =0;
    struct MarginCredit{
        bytes32 creditID;
        address spender;//保證金付款者
        bool isDefault;//是否違約
        bool isStrategy;//是否選定策略
        uint256 marginAmount;//保證金
        uint256 available_Amount;//可借額度=margin*4
        uint8 payrate;//利息/12 monthlyrate
        uint strategyID;//投資策略 
        //string symbol;
    }
    MarginCredit[]public margin_info;
    //憑證發行者
    address platform=0x63A40281087c53479382283dE03d64A83f5C7df0;
    address platform_token=0x98e2F1c551c05447BAe0a753f5b9fE243d8194b6;
    //address public minter;
    //mapping(address=>bool)minters;
    mapping(bytes32=>MarginCredit)public credits;
    //mapping(address =>uint256) _balances;
    mapping (bytes32 =>address)claimDeals;
    //event
    event RecordMint(bytes32 creditID,address borrower,bool Default,bool isStrategy,uint256 amount,uint256 available_amount,uint8 rate,uint strategyID);
    event Swap(address from, uint amount);
    event BurnCredit(bytes32 creditID,address borrower,uint amount);
    //modifier
    modifier BorrowerExist(address user){
        require(user!=address(0));
        _;
    }
    modifier onlyMinter(){
        require(msg.sender==platform);
        _;
    }
    /*
    modifier onlyOwner(address) {
        require(msg.sender == address());
        _;
    }
    */
    constructor() public{
        totalSupply_=Initial_Supply;
        //balances[borrower]=Initial_Supply;
        //platform=0x63A40281087c53479382283dE03d64A83f5C7df0;
        //minters[platform]=true;
        //totalSupply_=0;
        /*
         if (rate==2){
                if(CAM==true){
                    symbol="Credit_A1";
                }else{
                    symbol="Credit_A2";
                }
            }
            if(rate==5){
                if(CAM==true){
                    symbol="Credit_B1";
                }else{
                    symbol="Credit_B2";
                }
            }
            if(rate==10){
                if(CAM==true){
                    symbol="Credit_C1";
                }else{
                    symbol="Credit_C2";
                }
            }
            */
    }
    
      //mint credit
        function mint_margin_credit(address borrower,bool Default,bool isStrategy,uint256 amount,uint8 rate,uint strategyID)
        public
        BorrowerExist(borrower) 
        onlyMinter
        //returns(address)
        returns(bytes32,MarginCredit memory)
        {
            uint256 avail_amount=amount*4;
            totalSupply_=totalSupply_.add(avail_amount);
            balances[borrower]=balances[borrower].add(avail_amount);
            emit Transfer(address(0),borrower,avail_amount);
            //return true;
            //string memory credit_symbol;
            //credit_symbol=symbol;
            bytes32 creditID=bytes32(keccak256(abi.encodePacked(borrower,amount,strategyID)));
            MarginCredit memory receipt=MarginCredit(creditID,borrower,Default,isStrategy,amount,avail_amount,rate,strategyID);
            
            
            credits[creditID]=receipt;
            margin_info.push(credits[creditID]);
            emit RecordMint(creditID,borrower,Default,isStrategy,amount,avail_amount,rate,strategyID);
            return(creditID,credits[creditID]);
        }
        /*
        function getBalance(address addr_issue)public view returns(uint){
            ERC865Token token=ERC865Token(addr_issue);
            uint amount=token._balanceOf(msg.sender);
            return amount;
        }
        */
    //swap tokens
        function pay_credit(bytes32 creditID,uint amount)public payable returns(bool){
            require(credits[creditID].available_Amount>amount);
            credits[creditID].available_Amount=credits[creditID].available_Amount.sub(amount);
            balances[msg.sender]=balances[msg.sender].sub(amount);
            transfer_with_approve(msg.sender,platform,amount);
            require(change_to_token(creditID,amount));
            return true;
        }
        function change_to_token(bytes32 creditID,uint amount)internal returns(bool){
          //ERC865Token platform_token=ERC865Token(0x2Dccb2c99B63ee86cCd28A6C9369AA54439EBa61);
          address platform_token=0x98e2F1c551c05447BAe0a753f5b9fE243d8194b6;
          bytes memory do_swap=abi.encodeWithSignature("approve_and_transfer(address,address,uint256)",platform,msg.sender,amount);
	      platform_token.call(do_swap);
          //platform_token.transferFrom(platform,msg.sender,amount);
          emit Swap(msg.sender, amount);
          return true;
        }
    //brun credit
        function burn_margin_credit(bytes32 creditID,address borrower,uint256 amount)public returns(bool){
            require(amount<=balances[borrower]);
            require(msg.sender==platform);
            _approve(msg.sender,borrower,amount);//
            totalSupply_=totalSupply_.sub(amount);
            balances[borrower]=balances[borrower].sub(amount);
            delete credits[creditID];
            for(uint i=0;i<margin_info.length;i++){
                if(margin_info[i].creditID==creditID){
                    delete margin_info[i];
                }
            }
            emit Transfer(borrower,address(0),amount);
            emit BurnCredit(creditID,borrower,amount);
            return true;
        }
}
      