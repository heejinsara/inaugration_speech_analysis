## 트럼프와 바이든의 취임연설문 비교 분석 
2017년 트럼프의 취임연설문과 2021년 바이든의 취임연설문 수집하여 텍스트 분석을 진행했습니다.

## 1. 빈도분석
공통적으로 고빈도에 나타나는 어휘: america, american, people, nation, world, americans
- america와 관련된 어휘들이 많이 나타나는 것을 찾아볼 볼 수 있음.
- 미국 대통령 취임연설문이기 때문에 미국에 대한 정책이나 미국인에게 하고자 하는 말이 담겨져 있을 것
- america와 관련된 어휘에 대해서는 연어분석할 때 더 자세히 다룰 예정
## 2. bi-gram
![image](https://user-images.githubusercontent.com/91619301/204523339-f89269d4-c848-4554-875c-6ef8d106a947.png)

**🤓 Trump 고빈도 bi-gram**
- we will
  - will은 ‘할 것이다’라는 뜻으로 아직 사실이 확인되지 않았지만 미래에 확실한 사실로 나타날 것이라는 화자의 확신과 강력한 의지를 담고 있는 표현
  - we와 will을 함께 사용함으로써 Trump는 국민들과 함께 해나갈 것이라는 확고한 의지를 표현했다고 유추 가능
- of america, make america, american workers, bless america, america first 등 america와 관련된 어휘가 포함되는 bi-gram이 많이 나타남.

**😎 Biden 고빈도 bi-gram**
- we can
  - can은 ‘할 수 있다’라는 의미로 기본적으로 능력을 나타내며 용기를 심어주고 복돋아주는 표현
  - we와 can을 함께 사용함으로써 Biden은 국민들에게 할 수 있다는 용기를 복돋아주었다고 유추 가능
- fellow americans, my fellow
  - 국민을 가리킬 때 fellow를 많이 사용했음
- american story, fellow american, america has 등 america와 관련된 어휘를 포함되는 bi-gram이 많이 나타남.

## 3. 연어
**배경**
- america관련 어휘는 Trump와 Biden에서 모두 빈도가 높게 나타나고 bi-gram에서도 고빈도로 나타남.
- america관련 어휘는 어떤 어휘들과 함께 빈번하게 사용될까?
- Trump와 Biden이 각자 추진하고자 하는 미국정책이나 미국 국민들에게 말하고자 하는 이야기가 america관련 어휘 근처에서 연어분석을 통해 더 명확히 관찰할 수 있다고 생각
- america관련 어휘를 중심어로 설정하여 연어분석 진행

**조건**
- 중심어: America와 관련된 어휘들
- 중심어의 좌우 각각 두 구간씩, 총 4구간에 대해 살펴봄
- MI-Score 이용
  - MI-Score이 높을 때 텍스트 데이터를 해석하기 용이한 content word가 많이 출력되기 때문
![image](https://user-images.githubusercontent.com/91619301/204523671-7f3d5d53-caac-49d4-9810-712ed9311e09.png)

**🤓 Trump 중심어 근처에 MI-Score이 높게 나온 단어**

- workers
  - 트럼프가 미국의 노동자나 실업자를 주요대상으로 여기고 있다는 사실을 알 수 있음.
- make
  - 미국을 어떤 방향으로 만들지에 대해 이야기를 하는 것으로 유추 가능
- first
  - 미국 우선주의(American First)를 강조한 트럼프의 정책을 엿볼 수 있음.

**😎 Biden 중심어 근처에 MI-Score이 높게 나온 단어**

- fellow
  - Biden은 미국 자국민을 지칭할 때 fellow라는 어휘를 많이 사용
- story
  - 미국의 역사와 관련된 이야기를 했음을 유추 가능

## 4.키워드 분석
- comparision cloud 이용

![image](https://user-images.githubusercontent.com/91619301/204523779-de533247-c4c2-4d54-8589-c2f5f79a2644.png)

**🤓 Trump의 positive keyword**

- will
  - 확고한 의지를 나타내는 조동사를 많이 사용
- protected
  - 트럼프는 미국의 이익을 우선시한다는 background를 가지고 있다면 protected라는 어휘는 미국을 보호하겠다는 의미로 사용되었음을 짐작 가능
- jobs, workers
  - 근로자와 실업자, 이에 대한 일자리 관련돼서 이야기를 많이 함

**😎 Biden의 positive keyword**

- can
  - 할 수 있다는 능력을 나타내며 용기를 복돋우는 조동사를 많이 사용
- unity, uniting, democracy
  - Biden은 민주주의와 결합, 통합, 화합을 연설문에서 매우 강조했음을 알 수 있음.
- virus
  -  코로나가 계속 진행되는 상황 속에서 취임함

## 결론
Trump와 Biden의 취임연설문을 비교함으로써 각자 강조하는 내용이 무엇인지, 어떤 정책을 추구하는지, we will과 we can의 사용빈도 차이 등을 발견할 수 있었습니다.
